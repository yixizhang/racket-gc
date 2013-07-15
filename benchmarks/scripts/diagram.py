import os
import sys
import re
import numpy as np
import matplotlib.pyplot as plt
import parse


N = 8
ind = np.arange(N)
width = 0.20
colors = ['r', 'y', 'g']


def pattern(f):
    """ finid the pattern file matches
    pattern are embedded in file name
    """
    match = re.search('/\w+-', f)
    return match.group()[1:-1]


def sortbyname(files):
    """ sort files by number embedded in name """
    def filenumber(f):
        match = re.search('-\d+\.', f)
        return int(match.group()[1:-1])

    return sorted(files, key=filenumber)


def organize_data(files):
    """ sort out data into patterns """
    data = [{}, {}, {}, {}]

    files = sortbyname(files)
    for f in files:
        for i, d in enumerate(parse.parse_file(f)):
            pat = pattern(f)
            data[i].setdefault(pat, []).append(d)

    return data


def plot_graph(data, type, pats=None):
    """ plot diagram with x-axis in generation heap size ratio
    and y-axis in memory usage or collection pause running time
    """
    if pats is None:
        pats = data[0].keys()
    pats = sorted(pats)

    for index, d in enumerate(data):
        PLOT = plt.figure()
        ax = PLOT.add_subplot(111)

        patsdata = []
        for offset, pat in enumerate(pats):
            patsdata.append(ax.bar(ind+offset*width, d[pat], width,
                color=colors[offset]))

        if index == 0:
            ax.set_ylabel('Number of Cells')
            ax.set_title('Largest Mem Usage')
        else:
            ax.set_ylabel('Collection Running Time')
            if index == 1:
                ax.set_title('Longest Pause')
            elif index == 2:
                ax.set_title('Average Pause')
            elif index == 3:
                ax.set_title('Total Pause')
            
        ax.set_xticks(ind+width)
        if type == 'mutator':
            basedir = os.path.abspath('mutator')
            benchmarks = [f.split('.')[0] for f in os.listdir(basedir) if
                f.endswith('.rkt')]
            ax.set_xticklabels(tuple(benchmarks))
        elif type == 'heapsize':
            ax.set_xticklabels(('1/3', '1/4', '1/5', '1/6', '1/7', '1/8', '1/9', '1/10'))
        ax.legend(tuple(map(lambda x: x[0], patsdata)), pats)

        for rects in patsdata:
            for i, rect in enumerate(rects):
                height = rect.get_height()
                ax.text(rect.get_x()+rect.get_width()/2., 1.05*height, 
                    '%.2f' % (int(height) / float(patsdata[0][i].get_height())), 
                    ha='center', va='bottom')

    plt.show()


if __name__ == '__main__':
    basedir = os.path.abspath('mutators')

    for arg in sys.argv[1:]:
        if re.search('-type=', arg):
            types = arg.split('=')[1].split(',')
        if re.search('-b=', arg):
            benchmarks = arg.split('=')[1].split(',')
        if re.search('-p=', arg):
            pats = arg.split('=')[1].split(',')

    for type in types:
        if type == 'mutator':
            benchmarks = [f.split('.')[0] for f in os.listdir(basedir) if
                    f.endswith('.rkt')]
            files = []
            for b in benchmarks:
                targetdir = os.path.join(basedir, b)
                for f in os.listdir(targetdir):
                    if f.endswith('-3.rktd'):
                        files.append(os.path.join(targetdir, f))

        elif type == 'heapsize':
            for b in benchmarks:
                targetdir = os.path.join(basedir, b)
                files = [os.path.join(targetdir, f) for f in os.listdir(targetdir)
                    if f.endswith('.rktd')]

        data = organize_data(files)
        try:
            plot_graph(data, type, pats)
        except NameError:
            plot_graph(data, type)
