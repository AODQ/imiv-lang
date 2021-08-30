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

		CodeBlock < ((Message +) / Assignment)

		Message < "[" (Variable / Operator) (Request)* "];"

    AssignmentEq < ":="

		AssignmentKind < AssignmentEq

		Assignment < Variable TypeNotation? AssignmentKind Atom ";"

		TypeNotation < ":" Variable

		Integral < ~([0-9]+)
		StringLiteral <~ doublequote (!doublequote .)* doublequote
		Sign <- ('+'/'-')
		Integer <- Sign? Integral
		Float <- Sign? Integral '.' Integral 'f'
		Variable <- (alpha / Alpha) (alpha / Alpha / Operator / Integral)*
		Operator <- ("+" / "-" / "*" / "/" / "<")


    Params <- '|' (Variable ':' Variable ',')+ '|'
    Function <- Variable AssignmentEq 'Fn' ':' Variable '{' Params CodeBlock '}' ';'

		Array < "{" (Atom ",")* "}"

		Atom < (
			Array / Integer / Float / Variable / Operator / Message / StringLiteral
		)

		Request < (Variable) (':' (Atom))?
`));


void LogAssert(T...)(bool condition, string error, T params) {
	if (!condition)
		std.stdio.writefln("IMIV error: "~error, params);
	exit(0);
}

string ImivBuiltInPrint(ImivMessage message, const(ImivClass) * environment) {
	import std.format, std.sumtype, std.conv;

	static ImivObject printfRecursive = ImivObject();

	string str = "\n{ // -- printf\n";

	foreach (atom ; message.contents) {
		str ~= atom.value.match!(
			(ImivObject * object) => "N/A",
			(ImivClass * iclass) => "N/A",
			(ImivNil nil) => "", // just wants empty line
			(ImivFunction f) => "N/A",
			(ImivMessage msg) {
				return "
					{
						auto const ___printf_x = %s;
						printf(\"%s\", ___printf_x);
					}
				".format(
					TranspileAtomToExpression(atom, environment, false)
				);
			},
			(ImivValue value) {
				const string i = value.value.match!(
					(int i)    => i.to!string,
					(float i)  => i.to!string,
					(string i) => i.to!string,
				);

				return "printf(\"%%s\", %s);".format(i);
			},
			(ImivAtom[] atoms) {
				string str;
				foreach (atom ; atoms) {
					ImivMessage fakeMsg = {
						receiver : &printfRecursive,
						signature : ["printf"],
						contents : [atom],
					};
					str ~= ImivBuiltInPrint(fakeMsg, environment);
				}
				return str;
			},
		);
	}

	if (&printfRecursive != message.receiver) {
		str ~= "printf(\"\\n\");";
	}
	return str ~ "\n}\n";
}

struct ImivFunction {

	std.typecons.Tuple!(string, bool /*someplace in memory*/)[] messages;

	bool isBuiltInFunction = false;
	string function(ImivMessage, const(ImivClass) *) builtInFunction;
};

struct ImivValue {
	std.sumtype.SumType!(int, float, string) value;

	this(T)(T v) { value = v; }
};

struct ImivClass {
	ImivClass * parentclass;
	ImivClass[string] subclasses;
	ImivObject[string] variables;
	ImivFunction[string] functions;
	string label;
};

ImivAtom ImivAtomSymbolLookup(
	ImivClass * environment,
	immutable string symbolLabel
) {
	assert(environment);

	// look through variables first
	foreach (symbol, ref value; environment.variables) {
		if (symbol == symbolLabel) { return ImivAtom(&value); }
	}

	// finally look through parent
	if (environment.parentclass)
		return ImivAtomSymbolLookup(environment.parentclass, symbolLabel);

	return ImivAtom(ImivNil());
}

inout (ImivClass) * ImivClassLookup(
	inout(ImivClass) * environment,
	immutable string classLabel
) {
	assert(environment);
	std.stdio.writefln("comparing %s and %s", environment.label, classLabel);
	if (environment.label == classLabel)
		return environment;

	if (environment.parentclass)
		return ImivClassLookup(environment.parentclass, classLabel);

	return null;
}

struct ImivObject {
	ImivValue[string] variables;

	string label;
	ImivClass * objectClass;
};

struct ImivMessage {
	ImivObject * receiver;
	string[] signature;
	ImivAtom[] contents; // contents & labels must match
};

inout(string) FunctionLabelsJoiner(inout(string[]) labels) {
	import std.algorithm;
	import std.array;
	import std.conv;

	return labels.dup.joiner("___").array.to!string;
};

struct ImivNil {};

struct ImivAtom {
	import std.variant;
	std.sumtype.SumType!(
		ImivObject *,
		ImivClass *,
		ImivNil,
		ImivFunction,
		ImivMessage,
		ImivValue,
		ImivAtom[]
	) value;

	this (ImivAtom[] list) {
		value = list.dup;
	}

	this(T)(T t) {
		value = t;
	}
};

string ImivToDebugString(ImivAtom atom)
{
	import std.sumtype, std.conv;

	return atom.value.match!(
		(ImivObject * object)
			=> "(object '" ~ object.label ~ "' {" ~ object.objectClass.label ~ "}",
		(ImivClass * iclass) => "(class '" ~ iclass.label ~ "')",
		(ImivNil nil) => "(nil)",
		(ImivFunction func) => "(function)",
		(ImivMessage msg) {
			string str;
			assert(msg.receiver);
			str ~= "(message: '" ~ msg.receiver.label ~ "'";
			for (size_t it = 0; it < msg.signature.length; ++ it) {
				str ~=
					  " " ~ msg.signature[it]
					~ ":" ~ ImivToDebugString(msg.contents[it])
				;
			}
			str ~= ")";
			return str;
		},
		(ImivValue value) {
			return value.value.match!(
				(int i)    => i.to!string,
				(float i)  => i.to!string,
				(string i) => i,
			);
		},
		(ImivAtom[] atoms) {
			string str;
			str ~= "(list [";
			foreach (newAtom; atoms)
				str ~= ImivToDebugString(newAtom) ~ ", ";
			str ~= "])";
			return str;
		}
	);
}

auto ToString(T)(T t) {
	import std.algorithm;
	import std.array;
	import std.conv;
	return t.dup.joiner.array.to!string;
}

ImivAtom ComputeContents(T)(
	const(T) grammar,
	ImivClass * environment,
	immutable bool verbose
) {
	import std.algorithm;
	import std.array;
	import std.conv;
	import std.sumtype;

	switch (grammar.name) {
		default:
			std.stdio.writefln(
				"WARNING: unknown grammar name: '" ~ grammar.name ~ "'"
			);
			assert(false);
		case "PeggedGrammarImiv":
		case "PeggedGrammarImiv.CodeBlock":
		case "PeggedGrammarImiv.Array":
		case "PeggedGrammarImiv.Atom": {
			ImivAtom[] atoms;
			foreach (elem; grammar.children[0..$])
				atoms ~= ComputeContents(elem, environment, verbose);
			return ImivAtom(atoms);
		}
		case "PeggedGrammarImiv.Message": {
			ImivMessage message;

			// get the object that will receive the message
			auto receiverObject =
				ComputeContents(grammar.children[0], environment, verbose)
			;

			receiverObject.value.tryMatch!(
				(ImivObject * o) => message.receiver = o
			);

			// get the contents of the message to be received
			foreach (elem; grammar.children[1..$]) {

				immutable auto label = elem.children[0].matches[0..$].ToString;

				message.signature ~= label;

				// check if no value was passed into parameter
				if (elem.children.length == 1) {
					message.contents ~= ImivAtom(ImivNil());
					continue;
				}

				// variable was passed in
				message.contents ~=
					ComputeContents(elem.children[1], environment, verbose);
			}

			return ImivAtom(message);
		}

		case "PeggedGrammarImiv.Operator":
		case "PeggedGrammarImiv.Variable":
			return ImivAtomSymbolLookup(environment, grammar.matches[0..$].ToString);

		case "PeggedGrammarImiv.StringLiteral":
			return ImivAtom(ImivValue(grammar.matches[0..$].ToString));
	}

	assert(false);
}

ImivClass * DefaultEnvironment() {
	ImivClass * environment = new ImivClass; // best be on heap
	environment.label = "___ImivEnvironment";
	environment.parentclass = null;

	ImivClass subclass;
	subclass.label = "___Imiv";
	subclass.parentclass = environment;
	subclass.functions["println"] = ImivFunction();
	subclass.functions["println"].isBuiltInFunction = true;
	subclass.functions["println"].builtInFunction = &ImivBuiltInPrint;
	environment.subclasses[subclass.label] = subclass;

	ImivObject instance;
	instance.objectClass = &environment.subclasses[subclass.label];
	instance.label = "imiv";
	environment.variables["imiv"] = instance;

	return environment;
}

string TranspileAtomToExpression(
	ref ImivAtom atom,
	const(ImivClass) * environment,
	bool verbose
) {
	import std.sumtype, std.conv;

	return atom.value.match!(
		(ImivObject * object) => object.label,
		(ImivClass * iclass) => "",//??,
		(ImivNil nil) => "",//???
		(ImivFunction func) => "",//put in function declaration
		(ImivMessage msg) {
			assert(msg.receiver);

			const(ImivClass) * msgClass = msg.receiver.objectClass;
			assert(msgClass);

			const(ImivFunction) * func =
				msg.signature.FunctionLabelsJoiner in msgClass.functions
			;
			assert(func);

			if (func.isBuiltInFunction) {
				return func.builtInFunction(msg, environment);
			}

			return "";
		},
		(ImivValue value) {
			import std.conv;

			return value.value.match!(
				(int i)    => i.to!string,
				(float i)  => i.to!string,
				(string i) => i.to!string,
			);
		},
		(ImivAtom[] atoms) {
			std.stdio.writefln("atoms: %s" , ImivToDebugString(ImivAtom(atoms)));
			string str;
			str ~= "{ ";
			foreach (newAtom; atoms)
				str ~= TranspileAtomToExpression(newAtom, environment, verbose) ~ "; ";
			str ~= "}";
			return str;
		},
	);
}

string TranspileProgram(
	ref ImivAtom program,
	const(ImivClass) * environment,
	immutable bool verbose
) {

	import std.format;
	return "
		#include <stdio.h>
		#include <stdint.h>
		#include <string.h>

		struct imiv {
		};

		int main() {
			%s
		}
	".format(
		TranspileAtomToExpression(program, environment, verbose)
	);
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

	auto environment = DefaultEnvironment();
	scope(exit) environment.destroy();

	auto result = ComputeContents(expandedExpression, environment, verbose);

	if (verbose) {
		std.stdio.writefln(
			"computed expression:\n'''\n%s\n'''", ImivToDebugString(result)
		);
	}

	auto programString = TranspileProgram(result, environment, verbose);

	import std.process;

	std.file.write("test.c", programString);
	auto gcc = std.process.execute(["gcc", "test.c"]);

	if (gcc.status != 0) {
		std.stdio.writefln(
			"Compilation of the transpiled code failed:\n",
			gcc.output
		);
		return;
	}

	if (retainSourceFiles) {
		std.process.execute(
			["clang-format", "-i", "test.c", "-style=file"]
		);
	} else {
		std.file.remove("test.c");
	}

	if (verbose) {
		std.stdio.writefln("translated C code:\n'''");
		std.stdio.write(std.process.execute(["cat", "test.c"]).output);
		std.stdio.writefln("'''");
	}

	auto program = std.process.execute(["./a.out"]);
	std.stdio.writefln("---- program output ----\n");
	std.stdio.writef(program.output);

	if (!retainSourceFiles)
		std.file.remove("a.out");
}

string StripContents(immutable string expression, immutable bool verbose) {
	auto const trailingWhitespace = std.regex.regex(r" *\n");
	auto const comments = std.regex.regex(r"#.*\n");
	auto const newExpression =
		std.regex.replaceAll(
			std.regex.replaceAll(expression, comments, ""),
			trailingWhitespace, ""
		)
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

//	auto fileContents = std.file.readText(filename);
//	EvaluateContents(
//		StripContents(fileContents, verbose),
//		verbose,
//		retainSourceFiles
//	);

  util.FunctionCreateInfo addFunctionCreateInfo = {
    returnType : util.Type.i32,
    parameters : [util.Type.i32, util.Type.i32],
    label : "add",
  };
  auto mod = util.createModule("test-module");
  auto func = createFunction(mod, addFunctionCreateInfo);
}
