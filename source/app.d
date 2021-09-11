static import std.file;
static import std.getopt;
static import std.regex;
static import std.stdio;
static import std.sumtype;
static import std.typecons;

import pegged.grammar;

import util;

mixin(grammar(`
	PeggedGrammarImiv:

		CodeBlock < (CodeBlockStatement)+
		CodeBlockStatement < (Function / Message / Assignment / ReturnStatement)

		Message < "[" (Variable / Operator) (Spacing Request)+ "];"

		AssignmentEq < ":="

		AssignmentKind < AssignmentEq

		Assignment < Variable TypeNotation? AssignmentKind Atom ";"

		TypeNotation < ":" Variable

		Integral <- ([0-9]+)
		StringLiteral <~ doublequote (!doublequote .)* doublequote
		Sign <- ('+'/'-')
		Integer <- Sign? Integral
		Float <~ Sign? Integral '.' Integral 'f'
		Variable <- (alpha / Alpha) (alpha / Alpha)*
		Operator <- ("+" / "-" / "*" / "/" / "<")
		Type <- Variable
		ReturnStatement < 'ret' Message


		Params < '|' (Variable ':' Type ',')+ '|'
		Function <- Variable AssignmentEq Type Params '{' CodeBlock '}' ';'

		Array < "{" (Atom ",")* "}"

		Atom < (
			Array / Integer / Float / Variable / Operator / Message / StringLiteral
		)

		Request < (Variable / Integer) (TypeNotation)?
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
		GenericValue,
		Value,
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

	Value asValue() {
		import std.sumtype;
		Value v;
		this.value.tryMatch!((Value val) => v = val);
		return v;
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
			std.writefln("unknown value: '%s'", val);
		return *value;
	}
};

ImivAtom ComputeContents(T)(
	const(T) grammar,
	Context ctx,
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
		case "PeggedGrammarImiv.Integer":
			auto integer = grammar.matches[0..$].ToString.to!int;
			return ImivAtom(createValue(integer));

		case "PeggedGrammarImiv.ReturnStatement":
			auto retValue =
				ComputeContents(
					grammar.children[0], ctx, verbose
				)
			;
			Value value;
			retValue.value.tryMatch!((Value value_) => value = value_);
			ctx.instrBlock.buildRet(value);
		break;

		case "PeggedGrammarImiv.Function":
			util.FunctionCreateInfo funcCreateInfo = {
				returnType : util.Type.i32,
				parameters: [util.Type.i32, util.Type.i32],
				label : "add"
			};
			auto newFunc = createFunction(ctx.modul, funcCreateInfo);
			auto newInstrBlock = InstructionBlock.append(newFunc, "entry");
			auto codeBlock = grammar.children[4];

			Value[string] newNamedValues;

			// add parameters from the function to named values lookup
			for (uint it = 0; it < newFunc.getParameterLength(); ++ it) {
				auto label = grammar.children[3].children[it*2].matches[0..$].ToString;
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
			ComputeContents(codeBlock, newCtx, verbose);
		break;

		case "PeggedGrammarImiv.Request":
			return ComputeContents(grammar.children[0], ctx, verbose);

		case "PeggedGrammarImiv":
		case "PeggedGrammarImiv.CodeBlock":
		case "PeggedGrammarImiv.CodeBlockStatement":
		case "PeggedGrammarImiv.Array":
		case "PeggedGrammarImiv.Atom": {
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

		case "PeggedGrammarImiv.Message": {

			// assume + lol...
			std.writefln("params: %d", ctx.func.getParameterLength);
			auto label = grammar.children[0].matches[0..$].ToString;
			if (label == "+") {
				auto val0 = ctx.getNamedValue(grammar.children[1].matches[0..$].ToString);
				auto val1 = ctx.getNamedValue(grammar.children[2].matches[0..$].ToString);
				return ImivAtom(ctx.instrBlock.buildAdd(val0, val1, "+"));
			} else {
				auto callFunc = ctx.modul.getFunction(label);
				if (!callFunc.value) {
					std.writefln("could not get function from label '%s'", label);
				}
				auto callValue = 
					ctx.instrBlock.buildCall(
						callFunc,
						grammar
							.children[1..$]
							.map!(
								n =>
									ComputeContents(n, ctx, verbose).asValue
							)
							.array,
						"return-call-" ~ label
					)
				;
				std.writefln("building call... %s", callValue);
				return ImivAtom(callValue);
			}

			//if (
			/* ImivMessage message; */

			/* // get the object that will receive the message */
			/* auto receiverObject = */
			/* 	ComputeContents(grammar.children[0], verbose) */
			/* ; */

			/* receiverObject.value.tryMatch!( */
			/* 	(ImivObject * o) => message.receiver = o */
			/* ); */

			/* // get the contents of the message to be received */
			/* foreach (elem; grammar.children[1..$]) { */

			/* 	immutable auto label = elem.children[0].matches[0..$].ToString; */

			/* 	message.signature ~= label; */

			/* 	// check if no value was passed into parameter */
			/* 	if (elem.children.length == 1) { */
			/* 		message.contents ~= ImivAtom(ImivNil()); */
			/* 		continue; */
			/* 	} */

			/* 	// variable was passed in */
			/* 	message.contents ~= */
			/* 		ComputeContents(elem.children[1], verbose); */
			/* } */

			/* return ImivAtom(message); */
		}

		case "PeggedGrammarImiv.Operator":
		case "PeggedGrammarImiv.Variable":
			return ImivAtom(ctx.getNamedValue(grammar.matches[0..$].ToString));

		case "PeggedGrammarImiv.StringLiteral":
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
	auto expandedExpression = PeggedGrammarImiv(expression);

	if (verbose) {
		std.stdio.writefln("expanded expression:\n%s", expandedExpression);
	}

	auto modul = util.createModule("test-module");
	util.FunctionCreateInfo defFunctionCreateInfo = {
		returnType : util.Type.Void,
		parameters : [],
		label : "default-entry",
	};
	auto defFunc = createFunction(modul, defFunctionCreateInfo);
	auto instrBlock = InstructionBlock.append(defFunc, "entry");

	Context newCtx = {
		func : defFunc,
		instrBlock : instrBlock,
		modul : modul,
		namedValues : null
	};
	auto result = ComputeContents(expandedExpression, newCtx, verbose);
  instrBlock.buildRetVoid();

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

  /* util.FunctionCreateInfo addFunctionCreateInfo = { */
  /*   returnType : util.Type.i32, */
  /*   parameters : [util.Type.i32, util.Type.i32], */
  /*   label : "add", */
  /* }; */
  /* auto mod = util.createModule("test-module"); */
  /* auto addFunc = createFunction(mod, addFunctionCreateInfo); */

  /* auto instrBlock = InstructionBlock.append(addFunc, "entry"); */
  /* instrBlock.buildRet( */
  /*   instrBlock.buildAdd( */
  /*     addFunc.getParameter(0), */
  /*     addFunc.getParameter(1), */
  /*     "tmp-add" */
  /*   ) */
  /* ); */

}
