# Cargo Virtual Machine

For now this document will describe the Virtual Machine's possible structure and operation. It feels that it's necessary to arrange my thoughts, and see at least a general picture of the VM.

## CVM

### VM Structure

The machine will encapsulate the following entities as it's controlled components:

- text segment - array/list of program's instructions

- static memory segment, that will be constituted of:

  - bss - read/write memory segment. This segment is used for storage of uninitialized data. At assembly time it should be represented by it's overall size, and initialized with zeroes at VM startup.

  - data - read-only memory segment that is filled with constant data at assembly time.
  ```
  TODO: why the data memory segment must be protected from writing?
  ```

- operand stack - is used to pass arguments to instructions or functions and also to store their return results.

- call stack - is used to provide a free from interference memory place to store the stack frame of currently executing function. The stack frame layout will look like this:

  - `<return address>` - where to jump after finishing execution of a current function
  - `<function args>` - every argument that was passed through the operand stack will be moved onto call stack.
  - `<function local variables>` - initialized or uninitialized variables local to current function call
  - `[<frame pointer>]` - pointer to the base of current stack frame. It's only pushed onto call stack from eponymous machine register if some other function is called during the execution of the current one.

- set of registers to control the flow of the program's execution:

  - `ip` - instruction pointer register
  - `osp` - operand stack pointer register that points to the place of a next element in the `operand stack`
  - `csp` - call stack pointer, points on top of the call stack
  - `fp` - frame pointer, points at a base of current stack frame, and used as a base for accessing local variables and function's arguments using offset indices.

It might also have some dynamic linking/dispatching capabilities, e.g. at assembly stage the modules will not be linked statically, thus resulting in unresolved symbolic references. So there will be some kind of flag that will indicate that VM should run in dynamic fashion, and it will prepare
  - hash table, that will map *module names* to their corresponding program *instruction lists/arrays* packed together with its associated symbol table
    - that will map all of the program's symbolic references to specific instruction/memory addresses

So, little bit more formally, the VM will setup approximately the following chain of mapping structures:
```
ModuleName -> (SymbolicRef -> (InstAddress | MemAddress, Program, Memory))
where ->  = Hash map
       |  = Sum type
      (,) = Product type
```

Maybe the program itself will be a product of a static memory and instructions

```
Program = (Memory, Instructions)
```
It actually may be so, that the program's instructions and static memory will be initialized as a contiguous array. Then the VM will also need a bunch of registers that are going to store the information on the memory segments: text, data, bss or rather some kind of meta-information table that will associate specific access rights to each memory region of that overall memory array.

Thus, there is no need to distinguish between memory and instruction address spaces, so the chain of mappings will look like:
```
Program = Instructions × StaticMemory
ModuleName -> SymbolicRef -> Address × Program
```

In order to prevent `jmp` to static memory it's address space must be protected by additional runtime check: before changing the `ip` register to some address it will be tested to belong to the memory that hold the executable instructions. The same logic hold for data segment: before trying to write something to memory, the address will be tested for protection policies violation. Well, it actually may be generalized by assigning some policy bits to each address range of a program, and then before executing any 'unsafe' instruction it will be obligatory to check whether that operation is allowed or not.

The bss, data and text memory sizes are going to be known from the binary executable. Thus it is possible to setup the structures that will contain the bounds of each segment and their access policy information.

#### Segment Registers

VM will contain `segment_descriptor_table`, that will hold `segment_descriptor`s. Each `segment_descriptor` will contain `segment_size` `segment_access_policies` and `segment_pointer`. The `segment_pointer` is going to be used as a base relative to which the actual address is going to be calculated, e.g. `bss:12` in assembly will be like `segment_descriptor_table[bss].segment_pointer + 12`.

Segment descriptor mnemonics and their corresponding indices in the `segment-descriptor-table` that are going to be used in assembly language.

- `a | 0` - absolute address
- `t | 1` - text
- `d | 2` - data
- `b | 3` - bss
- `l | 4` - local

These values must/should be used in the assembly as prefixes to address values.

Since, only 48 bits of a virtual address are used for actual addressing - we're going to use highest 3 bits of a pointer to encode `segment-descriptor` index in the `segment-descriptor-table`.

- `111 | 000` - two bit patterns are going to be used to determine whether a pointer is an absolute address, because in [canonical](https://stackoverflow.com/questions/25852367/x86-64-canonical-address) form a virtual address will appear with all non-significant bits set either to zero or ones. Thus, for example, when foreign call to malloc will return a pointer value on the operand stack, it will already be treated as a valid address, that might be used with `read` and `write` operations.

- `001` - text segment descriptor 

- `010` - data segment descriptor  

- `011` - bss segment descriptor 

- `100` - local segment descriptor 

But, before dereferencing a pointer we need to bring it back to canonical form by setting all non-significant bits to the value of the highest significant bit of an address.
```c
addr <<= 16 // toggle address bits to the highest bits of the ptr
addr >>= 16 // shift filling all the bits by the value of the highest one
```

Read and write operations are going to consume all their input from the operand stack:
```
base_addr
offset
b_read | w_read | d_read | q_read

<base_addr>
<offset>
<value>
b_write | w_write | d_write | q_write
```

Read/write instructions for single values would not require any offset values.
```
<addr>
i_read | f_read

<addr>
<value>
i_write | f_write
```

Practically, we don't need any special operations to work with locals.
The only one that is necessary is the operation to declare a local variable.
It will simply work as a directive to the VM that will tell it to move the
call-stack-pointer one point up, so then it might be safely accessed by
default read and write operations

declare n // call_sp++


### VM Executable Binary File Format

The assembler must produce output that will describe the initial state of the vm.

The binary file may consist of the following parts:
- maybe some kind of magic bytes should be placed at the begining of a file to reduce the probability of running arbitrary files that were not meant to be executed on the vm
- entry point - either will contain an address of program's entry point, or it should tell that the program is not supposed to be run as an executable, in which case the exception must be raised by the vm. This field is going to be used to setup `ip` register of the machine.
- bss - uninitialized memory segment specification will consist only of its size in bytes
- data - will contain exact bytes that must be loaded in static memory array at the initialization stage.
- text - program's instructions

- symbol tables - can be used to disasemble the program in a more human readable form, and also, they can be utilized in dynamic linking process at runtime, to allow lookups in a hash table that might be maintained by the VM.
  - program symbols
  - per function symbol or local scope identifiers
  - memory references


### VM Runtime Description


#### initialization stage
On startup, vm will read contents of a program file that must conform to specific binary format.

#### execution loop
#### termination conditions

## CVM Assembler
### Assembly Language

## CVM Disassembler

## Build Instructions


## Usefull references

https://en.wikipedia.org/wiki/Virtual_machine

### Common Language Runtime
https://en.wikipedia.org/wiki/Common_Language_Runtime
https://en.wikipedia.org/wiki/Common_Intermediate_Language

> CIL bytecode has instructions for the following groups of tasks:
> 
> - Load and store
> - Arithmetic
> - Type conversion
> - Object creation and manipulation
> - Operand stack management (push / pop)
> - Control transfer (branching)
> - Method invocation and return
> - Throwing exceptions
> - Monitor-based concurrency
> - Data and function pointers manipulation needed for C++/CLI and unsafe C# code

https://en.wikipedia.org/wiki/Parrot_virtual_machine
https://en.wikipedia.org/wiki/Limbo_(programming_language)#Virtual_machine
https://en.wikipedia.org/wiki/Lua_(programming_language)#Internals
https://en.wikipedia.org/wiki/LLVM
https://en.wikipedia.org/wiki/Stack_machine
https://en.wikipedia.org/wiki/Register_machine
https://en.wikipedia.org/wiki/Java_virtual_machine
