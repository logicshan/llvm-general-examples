import LLVM.General
import LLVM.General.Analysis
import LLVM.General.Context
import Control.Monad.Error
import LLVM.General.Module
import LLVM.General.PrettyPrint

run src = do
  ll <- withContext $ \context ->
    runErrorT $ withModuleFromLLVMAssembly context src $ \mod -> do
      ast <- moduleAST mod
      return ast
  case ll of
    Left msg -> print msg
    Right mod -> putStrLn $ showPretty mod
l6 = "; ModuleID = 'top'"
l9 ="; ModuleID = 'top'\n\
   \ @0 = constant [13 x i8] c\"Hello World!\00\"\n\
   \ define i32 @main() {\n\
   \ entrypoint:\n\
   \ ret i32 0\n\
   \ }"
l8 = "; ModuleID = 'top'\n\
    \ define i32 @main() {\n\
    \ entrypoint:\n\
    \ ret i32 0\n\
    \ }"
l10="; ModuleID = 'top'\n\
   \ @0 = constant [13 x i8] c\"Hello World!\00\"\n\
   \ define i32 @main() {\n\
   \ entrypoint:\n\
   \ ret i32 0\n\
   \ }\n\
   \ declare i32 @puts(i8*)"

l12 = "\
\; ModuleID = 'top'\n\
\\n\
\@0 = private unnamed_addr constant [14 x i8] c\"hello world!\\0A\\00\"\n\
\\n\
\define i32 @main() {\n\
\entrypoint:\n\
\  %0 = call i32 @puts(i8* getelementptr inbounds ([14 x i8]* @0, i32 0, i32 0))\n\
\  ret i32 0\n\
\}\n\
\\n\
\declare i32 @puts(i8*)"

main :: IO ()
main = run l12
