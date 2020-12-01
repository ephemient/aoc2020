import io
import pkg_resources
import sys


def main():
    args = set(sys.argv[1:])
    days = pkg_resources.get_entry_map('aoc2020', 'aoc2020.days')
    for day, entry in sorted(days.items(), key=lambda item: int(item[0])):
        if args and day not in args:
            continue
        print(f'Day {day}')
        with io.TextIOWrapper(
                pkg_resources.resource_stream('aoc2020',
                                              f'day{day}.txt')) as fh:
            data = fh.readlines()
        for part in entry.load():
            print(part(data))
        print()


if __name__ == '__main__':
    main()
