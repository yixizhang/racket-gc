def parse_file(file):
    """ yield only int data from file """
    with open(file) as f:
        for l in f.readlines():
            if l[0] == ';' or l[0] == '(' or l[0] == '\n':
                pass
            else:
                try:
                    yield int(l[:-1])
                except Exception as e:
                    print l
                    raise e


def parse_files(files):
    """ return only int data from files """
    data = [[] for x in range(4)]

    for f in files:
        for i, d in enumrate(parse_data(f)):
            data[i].append(d)

    return data
