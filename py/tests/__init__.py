import io
import pkg_resources


class resources:
    def __getitem__(self, day):
        with io.TextIOWrapper(
                pkg_resources.resource_stream('aoc2020',
                                              f'day{day}.txt')) as fh:
            return fh.readlines()


resources = resources()
