module imiv_llvm;
import llvm;
import std;

struct Module { LLVMModuleRef value; }

Module createModule(string label) {
	return Module(LLVMModuleCreateWithName(label.ptr));
}

Function getFunction(Module self, string label) {
	auto value = LLVMGetNamedFunction(self.value, label.ptr);
	// TODO check value exists
	return Function(value);
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
	Void
};

private LLVMTypeRef toLlvmType(Type type) {
	switch (type) {
		default: assert(false);
		case Type.i32: return LLVMInt32Type();
		case Type.Void: return LLVMVoidType();
	}
}

alias GenericValue = LLVMGenericValueRef;

int ToI32(GenericValue value) {
	return cast(int)LLVMGenericValueToInt(value, true);
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

	return
		Function(
			LLVMAddFunction(
				mod.value,
				ci.label.ptr,

				LLVMFunctionType(
					ci.returnType.toLlvmType,
					ci.parameters.map!toLlvmType.array.ptr,
					cast(uint)ci.parameters.length,
					cast(LLVMBool) false // never variadic
				)
			)
		)
	;
}

struct InstructionBlock {
	LLVMBasicBlockRef block;
	LLVMBuilderRef builder;

	static InstructionBlock append
	(T)
	(
		ref T value,
		string label
	)
		if (is(typeof(T.value) == LLVMValueRef))
	{
		InstructionBlock ib = {
			block : LLVMAppendBasicBlock(value.value, label.ptr),
			builder : LLVMCreateBuilder,
		};
		LLVMPositionBuilderAtEnd(ib.builder, ib.block);
		return ib;
	}
};

struct Value {
	LLVMValueRef value;
};

Value buildAdd(
	ref InstructionBlock ib, Value v0, Value v1, string label
) {
	return Value(LLVMBuildAdd(ib.builder, v0.value, v1.value, label.ptr));
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
	Value[] values,
	string label
) {
	import std.algorithm, std.array;
	LLVMValueRef[] t = values.map!(v => v.value).array;
	return
		Value(
			LLVMBuildCall(
				ib.builder, fn.value, t.ptr, cast(uint)values.length, label.ptr
			)
		)
	;
}

////////////////////////////////////////////////////////////////////////////////

void WriteBitcode(Module self, string path) {
	if (LLVMWriteBitcodeToFile(self.value, path.ptr)) {
		std.writefln("Could not write bitcode to file: '%s'", path);
	}
}
