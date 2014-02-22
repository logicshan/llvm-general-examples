import LLVM.General.AST
import qualified LLVM.General.AST as AST
import LLVM.General.Context
import LLVM.General.Module
import Control.Monad.Error
import LLVM.General.AST.Global
import LLVM.General.AST.Instruction
import LLVM.General.AST.Operand
import LLVM.General.AST.Constant

initModule :: AST.Module
initModule = defaultModule {
  moduleName =  "top"
, moduleDefinitions = [func]
}

retTy :: Type
retTy = IntegerType 32

terminator :: Named Terminator
terminator = Do Ret {
  returnOperand = Just (ConstantOperand Int {
                           integerBits = 32,
                           integerValue = 0
                           }),
  metadata' = []
  }

blk :: BasicBlock
blk = BasicBlock (Name "entrypoint") [] terminator

func = GlobalDefinition $ functionDefaults {
  name = Name "main"
, returnType = retTy
, basicBlocks = [blk]
}

-- withContext :: (Context -> IO a) -> IO a
{-
withModuleFromAST
  :: Context
     -> AST.Module
     -> (LLVM.General.Module.Module -> IO a)
     -> Control.Monad.Trans.Error.ErrorT String IO a
-}
main :: IO ()
main = do
  s <- withContext $ \context ->
    runErrorT $ withModuleFromAST context initModule $ \m -> moduleLLVMAssembly m
  case s of
    Left err -> print err
    Right ll -> putStrLn ll
