module optimizetest;

/*

	if (optimize) {
		import llvm;
		auto manager = LLVMCreateFunctionPassManagerForModule(modul.value);
		//LLVMAddConstantMergePass(manager);
LLVMAddArgumentPromotionPass(manager);
LLVMAddConstantMergePass(manager);
LLVMAddCalledValuePropagationPass(manager);
LLVMAddDeadArgEliminationPass(manager);
LLVMAddFunctionAttrsPass(manager);
LLVMAddFunctionInliningPass(manager);
LLVMAddAlwaysInlinerPass(manager);
LLVMAddGlobalDCEPass(manager);
LLVMAddGlobalOptimizerPass(manager);
LLVMAddIPConstantPropagationPass(manager);
LLVMAddPruneEHPass(manager);
LLVMAddIPSCCPPass(manager);
LLVMAddStripDeadPrototypesPass(manager);
LLVMAddStripSymbolsPass(manager);

LLVMAddAggressiveDCEPass(manager);
LLVMAddAggressiveInstCombinerPass(manager);
LLVMAddBitTrackingDCEPass(manager);
LLVMAddAlignmentFromAssumptionsPass(manager);
LLVMAddCFGSimplificationPass(manager);
LLVMAddLateCFGSimplificationPass(manager);
LLVMAddDeadStoreEliminationPass(manager);
LLVMAddScalarizerPass(manager);
LLVMAddMergedLoadStoreMotionPass(manager);
LLVMAddGVNPass(manager);
LLVMAddNewGVNPass(manager);
LLVMAddIndVarSimplifyPass(manager);
LLVMAddInstructionCombiningPass(manager);
LLVMAddJumpThreadingPass(manager);
LLVMAddLICMPass(manager);
LLVMAddLoopDeletionPass(manager);
LLVMAddLoopIdiomPass(manager);
LLVMAddLoopRotatePass(manager);
LLVMAddLoopRerollPass(manager);
LLVMAddLoopUnrollPass(manager);
LLVMAddLoopUnswitchPass(manager);
LLVMAddLowerAtomicPass(manager);
LLVMAddMemCpyOptPass(manager);
LLVMAddPartiallyInlineLibCallsPass(manager);
LLVMAddLowerSwitchPass(manager);
LLVMAddPromoteMemoryToRegisterPass(manager);
LLVMAddReassociatePass(manager);
LLVMAddSCCPPass(manager);
LLVMAddScalarReplAggregatesPass(manager);
LLVMAddScalarReplAggregatesPassSSA(manager);
LLVMAddSimplifyLibCallsPass(manager);
LLVMAddTailCallEliminationPass(manager);
LLVMAddConstantPropagationPass(manager);
LLVMAddDemoteMemoryToRegisterPass(manager);
LLVMAddVerifierPass(manager);
LLVMAddCorrelatedValuePropagationPass(manager);
LLVMAddEarlyCSEPass(manager);
LLVMAddEarlyCSEMemSSAPass(manager);
LLVMAddLowerExpectIntrinsicPass(manager);
LLVMAddTypeBasedAliasAnalysisPass(manager);
LLVMAddScopedNoAliasAAPass(manager);
LLVMAddBasicAliasAnalysisPass(manager);
LLVMAddUnifyFunctionExitNodesPass(manager);
		//LLVMAddConstantPropagationPass(manager);
		LLVMAddInstructionCombiningPass(manager);
		assert(LLVMFinalizeFunctionPassManager(manager));
		LLVMDisposePassManager(manager);
		std.writefln("----***optimized***----");
		std.writefln("----------------------");
	}

*/
