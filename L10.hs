import LLVM.General.AST
import qualified LLVM.General.AST as AST
import LLVM.General.Context
import LLVM.General.Module
import Control.Monad.Error
import LLVM.General.AST.Global
import qualified LLVM.General.AST.Global as A.G
import LLVM.General.AST.Instruction
import LLVM.General.AST.Operand
import LLVM.General.AST.Constant
import LLVM.General.AST.Linkage
import LLVM.General.AST.Type
import LLVM.General.AST.AddrSpace
import Data.Char

initModule :: AST.Module
initModule = defaultModule {
  moduleName =  "top"
, moduleDefinitions = [helloworld, func, puts]
}

createGlobalString :: String -> Global
createGlobalString s = globalVariableDefaults {
  A.G.type' = ArrayType {nArrayElements = fromInteger l,elementType = IntegerType {typeBits = 8}}
, initializer = Just Array {
    memberType = IntegerType {typeBits = 8},
    memberValues = map trans s }}
  where
    l = toInteger $ length s
    trans c = Int {integerBits = 8, integerValue = toInteger $ ord c}

helloworld :: Definition
helloworld = GlobalDefinition $ (createGlobalString "hello world!\n\0") {
  name = UnName 0,
  isConstant = True,
  linkage = Internal,
  hasUnnamedAddr = True
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

puts = GlobalDefinition $ functionDefaults {
  name = Name "puts"
, returnType = retTy
, parameters = (
    [
      Parameter PointerType {
        pointerReferent = IntegerType {typeBits = 8},
        pointerAddrSpace = AddrSpace 0
      } (UnName 0) []
    ],
    False)
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
