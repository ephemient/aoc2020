from .machine import Instruction, Machine, Operation


def part1(lines):
    '''
    >>> part1(['nop +0', 'acc +1', 'jmp +4', 'acc +3', 'jmp -3', 'acc -99', 'acc +1', 'jmp -4', 'acc +6'])
    5
    '''
    seen = {0}
    for acc, ip in Machine(list(map(Instruction, lines))):
        if ip in seen:
            return acc
        seen.add(ip)
    return None


def part2(lines):
    '''
    >>> part2(['nop +0', 'acc +1', 'jmp +4', 'acc +3', 'jmp -3', 'acc -99', 'acc +1', 'jmp -4', 'acc +6'])
    8
    '''
    instructions = list(map(Instruction, lines))
    for instruction in instructions:
        operation = instruction.op
        if operation == Operation.NOP:
            flipped = Operation.JMP
        elif operation == Operation.JMP:
            flipped = Operation.NOP
        else:
            continue
        instruction.op = flipped
        seen = {0}
        for acc, ip in Machine(instructions):
            if ip in seen:
                break
            seen.add(ip)
        else:
            return acc
        instruction.op = operation


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
