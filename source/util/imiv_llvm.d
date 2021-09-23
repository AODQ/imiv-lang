module imiv_llvm;
import llvm;
import std;
static import std;

import std.string : toStringz;

struct Context {
	LLVMContextRef value;

	~this() {
		LLVMContextDispose(this.value);
	}
}

Context createContext() { return Context(LLVMContextCreate()); }

struct Module {
	LLVMModuleRef value;
}

Module createModule(string label) {
	return Module(LLVMModuleCreateWithName(label.toStringz));
}

string getName(T)(T value) if (is(T == Function) || is(T == Value)) {
	size_t len;
	const(char) * str = LLVMGetValueName2(value.value, &len);

	string buf;
	for (size_t i = 0; i < len; ++ i)
		buf ~= str[i];
	return buf;
}

void Dump(Module self) {
	std.writefln("---- module dump --------------------------");
	LLVMDumpModule(self.value);
	std.writefln("\n-------------------------------------------");
}

void Dump(Value self) {
	std.writefln("---- value  dump --------------------------");
	LLVMDumpValue(self.value);
	std.writefln("\n-------------------------------------------");
}

Function getFunction(Module self, string label) {
	auto value = LLVMGetNamedFunction(self.value, label.toStringz);
	// TODO check value exists
	return Function(value);
}

struct FunctionRange {
	LLVMValueRef iterator = null;
	LLVMModuleRef modul;

	this(LLVMModuleRef modul_) {
		this.modul = modul_;
		this.iterator = LLVMGetFirstFunction(this.modul);
	}

	bool empty() {
		return iterator == null;
	}

	Function front() {
		return Function(this.iterator);
	}

	void popFront() {
		this.iterator = LLVMGetNextFunction(this.iterator);
	}
}

FunctionRange functions(Module self) {
	return FunctionRange(self.value);
}

struct Attribute {
	LLVMAttributeRef value;
}

Attribute getStringAttributeFromKey(
	Function fn,
	LLVMAttributeIndex idx,
	string key
) {
	return
		Attribute(
			LLVMGetStringAttributeAtIndex(
				fn.value,
				idx,
				key.ptr, cast(uint)key.length
			)
		)
	;
}

bool verify(Module self) {
	char * error = null;
	LLVMVerifyModule(self.value, LLVMAbortProcessAction, &error);
	/* if (error) { */
	/*   std.writefln("ERROR: %s", error); */
	/*   return false; */
	/* } */
	LLVMDisposeMessage(error);
	return true;
}

Engine createEngineJIT(Module self) {
	Engine engine;
	//LLVMLinkInJIT();
	//LLVMInitializeNativeTarget();
	char * error;
	if (LLVMCreateExecutionEngineForModule(&engine.value, self.value, &error)) {
		std.writefln("ERR: failed to create execution engine");
	}

	if (error) {
		std.writefln("ERR: %s", error);
		LLVMDisposeMessage(error);
	}

	return engine;
}

struct Engine {
	LLVMExecutionEngineRef value;
};

GenericValue runFunction(Engine self, Function fn, GenericValue[] values) {
	return
		LLVMRunFunction(
			self.value, fn.value, cast(uint)values.length,
			values.array.ptr
		)
	;
}

enum Type {
	i32,
	Void,
};

enum TypeModifier {
	Const,
	Variable,
	Pointer,
};

TypeModifier toTypeModifier(string t) {
	final switch (t) {
		case "const": return TypeModifier.Const;
		case "var": return TypeModifier.Variable;
		case "*": return TypeModifier.Pointer;
	}
}

struct RealType {
	// int const * const ** (var ptr -> var ptr -> const ptr -> const int)
	// would be typeModifiers : [ptr, ptr, const, ptr, const]
	TypeModifier[] typeModifiers;
	Type baseType;

	// isVariable (not a register)
	bool isVariable() { return typeModifiers.length > 0; }
};

Type toType(string type) {
	switch (type) {
		default:
			assert(false, "could not convert '" ~ type ~ "' to type, unknown");
		case "i32": return Type.i32;
		case "void": return Type.Void;
	}
}

private LLVMTypeRef toLlvmType(Type type) {
	switch (type) {
		default: assert(false, "could not convert type for: " ~ type.to!string);
		case Type.i32: return LLVMInt32Type();
		case Type.Void: return LLVMVoidType();
	}
}

alias GenericValue = LLVMGenericValueRef;

int ToI32(GenericValue value) {
	return cast(int)LLVMGenericValueToInt(value, true);
}

Value ToConstI32(GenericValue value) {
	return
		Value(
			LLVMConstInt(
				toLlvmType(Type.i32),
				cast(ulong)ToI32(value),
				true
			)
		)
	;
}

// TODO instead of using T specialize on Type
GenericValue createValue(T)(T value) {
	static if(is(T == int32_t)) {
		return
			GenericValue(
				LLVMCreateGenericValueOfInt(
					toLlvmType(Type.i32),
					cast(ulong)value, true // signed
				)
			);
	} else {
		static assert(false);
	}
}

struct FunctionCreateInfo {
	Type returnType;
	Type[] parameters;
	string label;
	string[] attributes;
	// bool variadic; no support in IMIV
};

struct Function {
	LLVMValueRef value;
}

uint getParameterLength(ref Function f) {
	return LLVMCountParams(f.value);
}

Value getParameter(ref Function f, size_t index) {
	return Value(LLVMGetParam(f.value, cast(uint)index));
}

Function createFunction(ref Module mod, FunctionCreateInfo ci) {
	import std.algorithm;
	import std.array;

	Function fn =
		Function(
			LLVMAddFunction(
				mod.value,
				ci.label.toStringz,

				LLVMFunctionType(
					ci.returnType.toLlvmType,
					ci.parameters.map!toLlvmType.array.ptr,
					cast(uint)ci.parameters.length,
					cast(LLVMBool) false // never variadic
				)
			)
		)
	;

	ci.attributes.each!(
		(string attr) =>
			LLVMAddAttributeAtIndex(
				fn.value,
				0, // attribute index
				LLVMCreateStringAttribute(
					LLVMGetGlobalContext(),
					attr.toStringz, cast(uint)attr.length,
					"on", 2
				)
			)
	);

	return fn;
}

struct InstructionBlock {
	LLVMBasicBlockRef block;
	LLVMBuilderRef builder;
	int labelCounter = 0;

	immutable(char) * nextLabel() {
		auto label = std.conv.to!string(labelCounter);
		labelCounter += 1;
		return std.string.toStringz(label);
	}

	static InstructionBlock append(T)(ref T value, string label)
		if (is(typeof(T.value) == LLVMValueRef))
	{
		InstructionBlock ib = {
			block : LLVMAppendBasicBlock(value.value, label.toStringz),
			builder : LLVMCreateBuilder,
		};
		LLVMPositionBuilderAtEnd(ib.builder, ib.block);
		return ib;
	}
};

struct Value {
	LLVMValueRef value;
	RealType type;
};

Type typeOf(Value value) {
	if (LLVMTypeOf(value.value) == LLVMInt32Type())
		return Type.i32;
	if (LLVMTypeOf(value.value) == LLVMVoidType())
		return Type.Void;
	assert(false, "Could not get the type of value unknown");
}

enum OpEnum {
	AddInt, SubInt, DivInt, MulInt,
	AddFlt, SubFlt, DivFlt, MulFlt,
};

enum OpUnaryEnum {
	NegInt
};

OpEnum strToBuildOp(string op, bool isFloat) {
	switch (op) {
		default: assert(false, "unknown op: '" ~ op ~ "'");
		case "+": return isFloat ? OpEnum.AddFlt : OpEnum.AddInt;
		case "-": return isFloat ? OpEnum.SubFlt : OpEnum.SubInt;
		case "*": return isFloat ? OpEnum.MulFlt : OpEnum.MulInt;
		case "/": return isFloat ? OpEnum.DivFlt : OpEnum.DivInt;
	}
}

Value buildOpUnary(ref InstructionBlock ib, OpUnaryEnum op, Value v0) {
	assert(v0.value);
	final switch (op) {
		case OpUnaryEnum.NegInt:
			return Value(LLVMBuildNeg(ib.builder, v0.value, ib.nextLabel));
	}
}

Value buildOp(ref InstructionBlock ib, OpEnum op, Value v0, Value v1) {
	assert(v0.value, "nil v0");
	assert(v1.value, "nil v1");
	final switch (op) {
		case OpEnum.AddInt:
			return Value(LLVMBuildAdd(ib.builder, v0.value, v1.value, ib.nextLabel));
		case OpEnum.SubInt:
			return Value(LLVMBuildSub(ib.builder, v0.value, v1.value, ib.nextLabel));
		case OpEnum.MulInt:
			return Value(LLVMBuildMul(ib.builder, v0.value, v1.value, ib.nextLabel));
		case OpEnum.DivInt:
			return Value(LLVMBuildSDiv(ib.builder, v0.value, v1.value, ib.nextLabel));

		case OpEnum.AddFlt:
			return Value(LLVMBuildFAdd(ib.builder, v0.value, v1.value, ib.nextLabel));
		case OpEnum.SubFlt:
			return Value(LLVMBuildFSub(ib.builder, v0.value, v1.value, ib.nextLabel));
		case OpEnum.MulFlt:
			return Value(LLVMBuildFMul(ib.builder, v0.value, v1.value, ib.nextLabel));
		case OpEnum.DivFlt:
			return Value(LLVMBuildFDiv(ib.builder, v0.value, v1.value, ib.nextLabel));
	}
}

Value buildRet(
	ref InstructionBlock ib,
	Value v
) {
	return Value(LLVMBuildRet(ib.builder, v.value));
}

Value buildRetVoid(ref InstructionBlock ib) {
	return Value(LLVMBuildRetVoid(ib.builder));
}

Value buildCall(
	ref InstructionBlock ib,
	Function fn,
	Value[] values
) {
	import std.algorithm, std.array;
	assert(fn.value);
	LLVMValueRef[] t = values.map!(v => v.value).array;
	// TODO assert values.length matches expected llvm function parameters
	return
		Value(
			LLVMBuildCall(
				ib.builder,
				fn.value,
				t.ptr,
				cast(uint)values.length,
				ib.nextLabel
			)
		)
	;
}

Value buildAlloca(
	ref InstructionBlock ib, Type type, string label
) {
	return Value(LLVMBuildAlloca( ib.builder, type.toLlvmType, label.toStringz));
}

Value buildLoad(
	ref InstructionBlock ib, Value val
) {
	auto loadValue = LLVMBuildLoad(ib.builder, val.value, ib.nextLabel);
	assert(loadValue != null, "could not load value: " ~ val.to!string);
	std.writefln("value to load: '%s'", val.to!string);
	return Value(loadValue);
}

Value buildStore(
	ref InstructionBlock ib, Value dst, Value src
) {
	return Value(LLVMBuildStore(ib.builder, src.value, dst.value));
}

////////////////////////////////////////////////////////////////////////////////

void WriteBitcode(Module self, string path) {
	if (LLVMWriteBitcodeToFile(self.value, path.ptr)) {
		std.writefln("Could not write bitcode to file: '%s'", path);
	}
}
