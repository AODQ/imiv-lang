
import llvm;

struct Module { LLVMModuleRef value; }

Module createModule(string label) {
  return Module(LLVMModuleCreateWithName(label.str));
}

enum Type {
  i32,
};

private LLVMTypeRef toLlvmType(Type type) {
  switch (type) {
    default: assert(false);
    case Type.i32: return LLVMInt32Type();
  }
}

struct FunctionCreateInfo {
  Type returnType;
  Type[] parameters;
  string label;
  // bool variadic; no support in IMIV
};

struct Function { LLVMValueRef value; }

Function createFunction(ref Module mod, FunctionCreateInfo ci) {
	import std.algorithm;
	import std.array;

  LLVMTypeRef[] parameters = ci.parameters.map!toLlvmType;
  LLVMTypeRef returnType = ci.parameters.toLlvmType;
  return
    LLVMAddFunction(
      mod,
      ci.label,
      LLVMFunctionType(returnType),
      ci.parameters.map!toLlvmType.array,
      ci.parameters.length,
      0 // never variadic
    )
  ;
}
