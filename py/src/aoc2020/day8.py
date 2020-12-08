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
    stack = [(0, 0, set(), False)]
    while stack:
        acc, ip, seen, mutated = stack.pop()
        while ip not in seen:
            seen.add(ip)
            if ip not in range(len(instructions)):
                return acc
            instruction = instructions[ip]
            if instruction.op == Operation.ACC:
                acc += instruction.value
                ip += 1
            elif instruction.op == Operation.JMP:
                if not mutated:
                    stack.append((acc, ip + 1, set(seen), True))
                ip += instruction.value
            elif instruction.op == Operation.NOP:
                if not mutated:
                    stack.append(
                        (acc, ip + instruction.value, set(seen), True))
                ip += 1


parts = (part1, part2)

if __name__ == '__main__':
    lines = list(fileinput.input())
    print(part1(lines))
    print(part2(lines))
