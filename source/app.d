static import std.file;
static import std.getopt;
static import std.regex;
static import std.stdio;
static import std.sumtype;
static import std.typecons;

import pegged.grammar;

import util;

mixin(grammar(`
	PGI:

		CodeBlock < (CodeBlockStatement)+
		CodeBlockStatement < (Function / Message / Assignment / ReturnStatement) ";"

		Message < "[" (Variable / Operator) (Spacing Request)+ "]"

		AssignmentEq < ":="

		AssignmentKind < AssignmentEq

		TypeModifier < ( "const" / "var" )

		Assignment < (TypeModifier+) Variable AssignmentKind TypeNotation? '{' Atom '}'

		TypeNotation < Variable

		Integral <- ([0-9]+)
		StringLiteral <~ doublequote (!doublequote .)* doublequote
		Sign <- ('+'/'-')
		Integer <- Sign? Integral
		Float <~ Sign? Integral '.' Integral 'f'
		Variable <- (alpha / Alpha) (alpha / Alpha / [0-9])*
		Operator <- ("+" / "-" / "*" / "/" / "<")
		Type <- Variable
		ReturnStatement < 'ret' (Atom)


		Params < '|' (Variable ':' Type ',')+ '|'
		Function < "fn" Variable AssignmentEq Type Params '{' CodeBlock '}'

		Array < "{" (Atom ",")* "}"

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
			assert(grammar.children[0].name == "PGI.TypeModifier");
			auto typeModifiers = grammar.children[0];
			assert(grammar.children[1].name == "PGI.Variable");
			auto variableLabel = grammar.children[1].matches[0..$].ToString;
			assert(grammar.children[2].name == "PGI.AssignmentKind");
			auto assignmentKind = grammar.children[2].matches[0..$].ToString;
			// TODO this is option(spacing, typenotation, spacing) ...
			assert(grammar.children[3].name == "PGI.TypeNotation");
			std.writefln("type: %s", grammar.children[3]);
			std.writefln("type1: %s", grammar.children[3].children[0]);
			auto typeNotation =
				grammar.children[3].children[0].matches[0..$].ToString
			;
			assert(grammar.children[4].name == "PGI.Atom");
			auto results = ComputeContents(grammar.children[4], ctx, verbose);

			// -- see if we can assign
			if (variableLabel in ctx.namedValues) {
				assert(
					typeModifiers.children.length == 0,
					"declaration shadows existing var"
				);
				ctx.instrBlock.buildStore(
					ctx.namedValues[variableLabel],
					results.asValue(ctx)
				);
			}

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
			ctx.instrBlock.buildStore(allocaValue, results.asValue(ctx));

			return ImivAtom(allocaValue);

		case "PGI.Function":
			//Function < "fn" Variable AssignmentEq Type Params '{' CodeBlock '}' ';'
			auto const functionLabel = grammar.children[0].matches[0..$].ToString;
			auto const returnType = grammar.children[2].matches[0..$].ToString;
			auto const parameters = grammar.children[3].children;
			auto const codeblock = grammar.children[4];

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
				label : functionLabel
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

			// store into current context
			//ctx.namedValues[
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
	immutable bool retainSourceFiles
) {
	auto expandedExpression = PGI(expression);

	if (verbose) {
		std.stdio.writefln("expanded expression:\n%s", expandedExpression);
	}

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

	auto value = engine.runFunction(defFunc, []);

	std.writefln("results: %d", value.ToI32);

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
	auto helpInformation = std.getopt.getopt(
		args,
		"file",    &filename, //  string
		"verbose", &verbose,  // flag
		"retain-files", &retainSourceFiles,  // flag
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
		retainSourceFiles
	);
}
