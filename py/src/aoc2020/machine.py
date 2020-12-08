from dataclasses import dataclass
from enum import Enum


class Operation(Enum):
    ACC = 1
    JMP = 2
    NOP = 3


@dataclass
class Instruction:
    op: Operation
    value: int

    def __init__(self, line):
        if line.startswith('acc '):
            self.op = Operation.ACC
        elif line.startswith('jmp '):
            self.op = Operation.JMP
        elif line.startswith('nop '):
            self.op = Operation.NOP
        else:
            raise ValueError(f'Invalid instruction: {line!r}')
        self.value = int(line[4:].removeprefix('+').rstrip())

    def __iter__(self):
        return iter((self.op, self.value))


class Machine:
    def __init__(self, instructions):
        self._instructions = instructions
        self._acc = 0
        self._ip = 0

    def __iter__(self):
        return self

    def __next__(self):
        if self._ip not in range(len(self._instructions)):
            raise StopIteration()
        op, value = self._instructions[self._ip]
        if op == Operation.NOP:
            self._ip += 1
        elif op == Operation.JMP:
            self._ip += value
        elif op == Operation.ACC:
            self._acc += value
            self._ip += 1
        else:
            raise ValueError(f'Invalid operation: {operation!r}')
        return self._acc, self._ip
