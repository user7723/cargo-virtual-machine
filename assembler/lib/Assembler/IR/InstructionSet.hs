module Assembler.IR.InstructionSet where

data Inst
  = Nop            -- nullary : () -> ()
  | Push           -- nullary : ∀t.() -> (t)
  | Drop           -- nullary : ∀t.(t) -> (t)
  | Icmp           -- nullary : (i64, i64) -> (i64)
  | Jmp_if         -- unary   : (i64) -> ()
  | Jeq            -- unary   : (i64) -> ()
  | Jle            -- unary   : (i64) -> ()
  | Jmp            -- unary   : () -> ()
  -- call instruction is going to accept amount of arguments that
  -- are must be moved from the operand stack to the call stack and
  -- an address where it is supposed to jump at
  | Call           -- unary   : () -> ()
  | Ret            -- nullary : () -> ()
  | Imul           -- nullary : (i64, i64) -> (i64)
  | Idiv           -- nullary : (i64, i64) -> (i64)
  | Iadd           -- nullary : (i64, i64) -> (i64)
  | Isub           -- nullary : (i64, i64) -> (i64)

-- Local Scope Instructions
  -- decl - occupies place at the call stack for a local variable
  | Decl          -- nullary

-- VM Memory Instructions
  | Read          -- nullary : (addr:i64) -> (i64)
  | Read_a64      -- nullary : (addr:i64, off:i64) -> (i64)
  | Write         -- nullary : (addr:i64, val:i64) -> ()
  | Write_a64     -- nullary : (addr:i64, off:i64, val:i64) -> ()
  deriving (Show, Enum, Bounded)
