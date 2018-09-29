import Text.Regex
import Data.List (maximum)
import qualified Data.Map as Map

type ArithOp = Int -> Int
type CondOp = Int -> Bool
type Instruction = (String, ArithOp, String, CondOp)
type RegisterMap = Map.Map String Int

readInt :: String -> Int
readInt val = read val::Int

parseArithInstr :: String -> (Int -> Int -> Int)
parseArithInstr "dec" = (-)
parseArithInstr "inc" = (+)

parseCondInstr :: String -> (Int -> Int -> Bool)
parseCondInstr ">=" = (>=)
parseCondInstr "<=" = (<=)
parseCondInstr "==" = (==)
parseCondInstr "!=" = (/=)
parseCondInstr "<" = (<)
parseCondInstr ">" = (>)

parseInstruction :: String -> Instruction
parseInstruction line = (modReg, operation, condReg, condition)
  where lineRegex = mkRegex "([a-z]+) (dec|inc) (-?[0-9]+) if ([a-z]+) ([=<>!]+) (-?[0-9]+)"
        Just [modReg, modIns, modVal, condReg, condIns, condVal] = matchRegex lineRegex line
        operation x = parseArithInstr modIns x (readInt modVal)
        condition x = parseCondInstr condIns x (readInt condVal)

executeFrom :: RegisterMap -> [Instruction] -> [RegisterMap]
executeFrom = scanl process
  where process registers (modReg, operation, condReg, condition) =
          let condRegVal = Map.findWithDefault 0 condReg registers
              modRegVal = Map.findWithDefault 0 modReg registers
          in if condition condRegVal then Map.insert modReg (operation modRegVal) registers
                                     else registers

maxRegVal :: RegisterMap -> Int
maxRegVal registers
  | null registers = 0
  | otherwise =  maximum . Map.elems $ registers

part1 :: [Instruction] -> Int
part1 = maxRegVal . last . executeFrom Map.empty

part2 :: [Instruction] -> Int
part2 instructions = maximum $ map maxRegVal (executeFrom Map.empty instructions)

main = do
  input <- readFile "day8-input.txt"
  let instructions = map parseInstruction (lines input)
  print $ part1 instructions
  print $ part2 instructions
