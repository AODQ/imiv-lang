static import std.conv;
static import std.file;
static import std.getopt;
static import std.regex;
static import std.stdio;
static import std.sumtype;
static import std.typecons;

import pegged.grammar;

import util;

void assertGrammarName(T)(T grammar, string expectation) {
	if (grammar.name != expectation) {
		std.writefln(
			"Expecting grammar: %s, but received: %s",
			expectation, grammar.name
		);
		assert(false);
	}
}

mixin(grammar(`
	PGI:

		CodeBlock < (CodeBlockStatement)+
		CodeBlockStatement < (Function / Message / Assignment / ReturnStatement) ";"

		Message < "[" (Variable / Operator) (Spacing Request)* "]"

		AssignmentEq < ":="

		AssignmentKind < AssignmentEq

		TypeModifier < ( "const" / "var" )

		Assignment < (TypeModifier)* Variable AssignmentKind TypeNotation? '{' Atom '}'

		TypeNotation < Variable

		Integral <- ([0-9]+)
		StringLiteral <~ doublequote (!doublequote .)* doublequote
		Sign <- ('+'/'-')
		Integer <- Sign? Integral
		Float <~ Sign? Integral '.' Integral 'f'
		Variable <- (alpha / Alpha) (alpha / Alpha / '-' / '_' / [0-9])*
		Operator <- ("+" / "-" / "*" / "/" / "<")
		Type <- Variable
		ReturnStatement < 'ret' (Atom)


		Params < '[' (Variable ':' Type ',')+ ']'
		FnParams < '|' (Variable ':' Type ',')* '|'
		Function < Array? "fn" Variable AssignmentEq Type FnParams '{' CodeBlock '}'

		Array < "[" (Atom ",")* "]"

		Atom < (
			Array / Integer / Float / Variable / Operator / Message / StringLiteral
		)

		Request < (Atom) (":" TypeNotation)?
`));


void LogAssert(T...)(bool condition, string error, T params) {
	if (!condition)
		std.stdio.writefln("IMIV error: "~error, params);
	exit(0);
}

struct ImivNil {};

struct ImivAtom {
	import std.variant;
	std.sumtype.SumType!(
		util.GenericValue,
		util.Value,
		ImivNil,
		/* ImivObject *, */
		/* ImivClass *, */
		/* ImivNil, */
		/* ImivFunction, */
		/* ImivMessage, */
		/* ImivValue, */
		ImivAtom[],
	) value;

	this (ImivAtom[] list) {
		value = list.dup;
	}

	this(T)(T t) {
		value = t;
	}

	util.Value asValue(Context ctx) {
		import std.sumtype;
		Value v;
		this.value.tryMatch!(
			(util.Value val) => v = val,
			(util.GenericValue val) => v = val.ToConstI32,
		);
		return v;
	}

	util.Value toRegisterValues(Context ctx) {
		import std.sumtype;
		return this.value.tryMatch!(
			(util.Value val) {
				if (val.type.isVariable)
					return ctx.instrBlock.buildLoad(val);
				return val;
			},
			(util.GenericValue val) => val.ToConstI32,
		);
	}
};

auto ToString(T)(T t) {
	import std.algorithm;
	import std.array;
	import std.conv;
	return t.dup.joiner.array.to!string;
}

struct Context {
	Function func;
	InstructionBlock instrBlock;
	Module modul;
	Value[string] namedValues;

	Value getNamedValue(string val) {
		auto value = val in this.namedValues;
		if (!value)
			std.writefln("unknown value: '%s', from list: %s", val, this.namedValues);
		return *value;
	}
};

ImivAtom ComputeContents(T)(
	const(T) grammar,
	ref Context ctx,
	immutable bool verbose,
) {
	assert(grammar.successful, "grammar not success");
	import std.algorithm;
	import std.array;
	import std.conv;
	import std.sumtype;

	std.stdio.writefln("processing: %s", grammar.name);

	switch (grammar.name) {
		default:
			std.stdio.writefln(
				"WARNING: unknown grammar name: '" ~ grammar.name ~ "'"
			);
			assert(false);
		case "PGI.Integer":
			auto integer = grammar.matches[0..$].ToString.to!int;
			return ImivAtom(createValue(integer));

		case "PGI.ReturnStatement":
			auto retValue =
				ComputeContents(
					grammar.children[0], ctx, verbose
				)
			;
			ctx.instrBlock.buildRet(retValue.toRegisterValues(ctx));
		break;

		case "PGI.Assignment":
			// Assignment <
			//   (TypeModifier+) Variable AssignmentKind TypeNotation? '{' Atom '}'
			auto it = 0;
			bool isDeclaration = false;
			auto typeModifiers = grammar.children[it];
			if (grammar.children[it].name == "PGI.TypeModifier") {
				isDeclaration = true;
				++ it;
			}
			assertGrammarName(grammar.children[it], "PGI.Variable");
			auto variableLabel = grammar.children[it].matches[0..$].ToString;
			++ it;

			assertGrammarName(grammar.children[it], "PGI.AssignmentKind");
			auto assignmentKind = grammar.children[it].matches[0..$].ToString;
			++ it;

			bool hasTypeDeclaration = false;
			string typeNotation = "";
			if (grammar.children[it].name == "PGI.TypeNotation") {
				hasTypeDeclaration = true;
				std.writefln("type: %s", grammar.children[it]);
				std.writefln("type1: %s", grammar.children[it].children[0]);
				typeNotation =
					grammar.children[it].children[0].matches[0..$].ToString
				;
				++ it;
			}

			std.writefln("grammar name: %s", grammar.children[it]);
			assertGrammarName(grammar.children[it], "PGI.Atom");
			auto results = ComputeContents(grammar.children[it], ctx, verbose);

			// -- see if we can assign
			if (variableLabel in ctx.namedValues) {
				std.writefln("found variable: '%s'", variableLabel);
				assert(!isDeclaration, "variable shadowing: '" ~ variableLabel ~ "'");
				assert(
					typeModifiers.children.length == 0,
					"declaration shadows existing var"
				);
				ctx.instrBlock.buildStore(
					ctx.namedValues[variableLabel],
					results.toRegisterValues(ctx)
				);
				return ImivAtom(ctx.namedValues[variableLabel]);
			}

			std.writefln("will create variable: '%s'", variableLabel);

			// -- create new variable
			// allocate variable to stack
			auto allocaValue =
				ctx.instrBlock.buildAlloca(
					typeNotation.toType,
					variableLabel
				)
			;
			allocaValue.type.typeModifiers =
				[typeModifiers.matches[0..$].ToString.toTypeModifier]
				/* typeModifiers.map!(n => n.matches[0..$].ToString.toTypeModifier) */
			;
			allocaValue.type.baseType = typeNotation.toType;
			std.writefln("type mods : %s", typeModifiers);
			std.writefln("type : %s", allocaValue.type);
			assert(
				allocaValue.type.isVariable,
				"could not create var: '" ~ variableLabel ~ "'"
			);

			std.writefln("inserting variable: '%s'", variableLabel);
			ctx.namedValues[variableLabel] = allocaValue;

			// assign value
			assert(assignmentKind == ":=", "only support ':=' for now");
			ctx.instrBlock.buildStore(allocaValue, results.toRegisterValues(ctx));

			return ImivAtom(allocaValue);

		case "PGI.Function":
			// Function <
			//   Array? "fn" Variable AssignmentEq Type Params '{' CodeBlock '}'
			size_t idx = 0;
			auto const arrayGrammar = grammar.children[idx];
			bool hasArrayGrammar = false;
			if (grammar.children[0].name == "PGI.Array") {
				hasArrayGrammar = true;
				++ idx;
			}

			assertGrammarName(grammar.children[idx], "PGI.Variable");
			auto const functionLabel = grammar.children[idx].matches[0..$].ToString;
			++ idx;
			assertGrammarName(grammar.children[idx], "PGI.AssignmentEq");
			++ idx;
			assertGrammarName(grammar.children[idx], "PGI.Type");
			auto const returnType = grammar.children[idx].matches[0..$].ToString;
			++ idx;
			assertGrammarName(grammar.children[idx], "PGI.FnParams");
			auto const parameters = grammar.children[idx].children;
			++ idx;
			assertGrammarName(grammar.children[idx], "PGI.CodeBlock");
			auto const codeblock = grammar.children[idx];

			assert(codeblock.name == "PGI.CodeBlock");

			// params => {var, type, var, type, ...}
			auto const parameterReturnTypes =
				parameters.filter!(l => l.name == "PGI.Type").array
			;
			auto const parameterLabels =
				parameters.filter!(l => l.name == "PGI.Variable").array
			;

			util.FunctionCreateInfo funcCreateInfo = {
				returnType : returnType.toType,
				parameters:
					parameterReturnTypes.map!(l => l.matches[0..$].ToString.toType).array,
				label : functionLabel,
				attributes :
					hasArrayGrammar
						? arrayGrammar
						  	.children[0..$]
						  	.map!(n => n.matches[0..$].ToString)
						  	.array
						: []
			};
			auto newFunc = createFunction(ctx.modul, funcCreateInfo);
			auto newInstrBlock = InstructionBlock.append(newFunc, "entry");

			Value[string] newNamedValues;

			// add parameters from the function to named values lookup
			for (uint it = 0; it < newFunc.getParameterLength(); ++ it) {
				auto label = parameterLabels[it].matches[0..$].ToString;
				std.writefln("label parameter: %s", label);
				newNamedValues[label] = newFunc.getParameter(it);
			}

			// create a new context for this function
			Context newCtx = {
				func : newFunc,
				instrBlock : newInstrBlock,
				modul : ctx.modul,
				namedValues : newNamedValues
			};
			ComputeContents(codeblock, newCtx, verbose);
		break;

		case "PGI.Atom":
		case "PGI.Request":
			return ComputeContents(grammar.children[0], ctx, verbose);

		case "PGI":
		case "PGI.CodeBlock":
		case "PGI.CodeBlockStatement":
		case "PGI.Array":
		{
			foreach (elem ; grammar.children[0..$]) {
				auto results = ComputeContents(elem, ctx, verbose);
				//results.tryMatch!(
				//	(Value val) {
				//
				//	},
				//);
				//this.value.tryMatch!((Value val) => v = val);
				//if (val
			}
			/* ImivAtom[] atoms; */
			/* foreach (elem; grammar.children[0..$]) */
			/* 	atoms ~= ComputeContents(elem, verbose); */
			/* return ImivAtom(atoms); */
		}
		break;

		case "PGI.Message": {

			std.writefln("params: %s", grammar.children[1..$]);
			auto label = grammar.children[0].matches[0..$].ToString;

			auto parameters =
				grammar
					.children[1..$]
					.map!(
						n =>
							ComputeContents(n, ctx, verbose).toRegisterValues(ctx)
					)
					.array
			;

			if (label == "+" || label == "-" || label == "/" || label == "*") {
				util.OpEnum resolvedOp = util.strToBuildOp(label, false);
				assert(parameters.length == 2, "only can do 2 params for now");
				std.writefln("calling: %s for params: %s", label, parameters);
				return
					ImivAtom(
						ctx.instrBlock.buildOp(resolvedOp, parameters[0], parameters[1])
					)
				;
			} else {
				auto callFunc = ctx.modul.getFunction(label);
				assert(
					callFunc.value,
					"could not get function from label '" ~ label ~ "'"
				);
				auto callValue = ctx.instrBlock.buildCall(callFunc, parameters);
				std.writefln("building call... %s", callValue);
				return ImivAtom(callValue);
			}
		}

		case "PGI.Operator":
		case "PGI.Variable":
			return ImivAtom(ctx.getNamedValue(grammar.matches[0..$].ToString));

		case "PGI.StringLiteral":
			/* return ImivAtom(ImivValue(grammar.matches[0..$].ToString)); */
		break;
	}

	return ImivAtom(ImivNil());
}

void EvaluateContents(
	immutable string expression,
	immutable bool verbose,
	immutable bool retainSourceFiles,
	immutable bool runUnitTests
) {
	auto expandedExpression = PGI(expression);
	assert (
		expandedExpression.successful,
		  "expression not successful: '"
		~ std.conv.to!string(expandedExpression) ~ "'"
	);

	if (verbose) {
		std.stdio.writefln("expanded expression:\n%s", expandedExpression);
	}

	util.createContext();
	auto modul = util.createModule("test-module");
	util.FunctionCreateInfo defFunctionCreateInfo = {
		returnType : util.Type.i32,
		parameters : [],
		label : "default-entry",
	};
	auto defFunc = createFunction(modul, defFunctionCreateInfo);
	auto instrBlock = InstructionBlock.append(defFunc, "entry");

	Context newCtx = {
		func : defFunc,
		instrBlock : instrBlock,
		modul : modul,
		namedValues : null,
	};
	auto result = ComputeContents(expandedExpression, newCtx, verbose);

	modul.DumpModule;
	modul.verify;

	auto engine = modul.createEngineJIT();

	modul.WriteBitcode("out.bc");

	if (!runUnitTests) {
		auto value = engine.runFunction(defFunc, []);
		std.writefln("results: %d", value.ToI32);
	} else {
		// iterate functions & check for unittest attribute
		foreach (fn; modul.functions) {
			// check if unittest
			bool isUnitTest =
				fn.getStringAttributeFromKey(0, "unittest").value != null
			;
			if (isUnitTest) {
				std.writefln("test res: %d", engine.runFunction(fn, []).ToI32);
			}
		}
	}

	std.writefln("exiting imiv");
}

string StripContents(immutable string expression, immutable bool verbose) {
	//auto const trailingWhitespace = std.regex.regex(r" *\n");
	auto const comments = std.regex.regex(r"#.*\n");
	auto const newExpression =
		//std.regex.replaceAll(
			std.regex.replaceAll(expression, comments, "")
			//trailingWhitespace, ""
		//)
	;
	if (verbose) {
		std.stdio.writefln("stripped contents:\n'%s'", newExpression);
	}
	return newExpression;
}

void main(string[] args) {
	string filename = "";
	bool verbose = false;
	bool retainSourceFiles = false;
	bool runUnitTests = false;
	auto helpInformation = std.getopt.getopt(
		args,
		"file",    &filename, //  string
		"verbose", &verbose,  // flag
		"retain-files", &retainSourceFiles,  // flag
		"run-unit-tests", &runUnitTests,  // flag
	);

	if (helpInformation.helpWanted || filename == "") {
		std.getopt.defaultGetoptPrinter(
			"IMIV - Virtually Integrated Monkeys, \\i/",
			helpInformation.options
		);
		return;
	}

	auto fileContents = std.file.readText(filename);
	EvaluateContents(
		StripContents(fileContents, verbose),
		verbose,
		retainSourceFiles,
		runUnitTests
	);
}
