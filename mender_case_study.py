# coding: utf-8

def main():
    # 1
    dict1 = {}
    dict2 = {}
    with open("fake_input.csv", 'r') as fin:
        next(fin)
        for line in fin:
            try:
                i1 = line.find(",")
                hasFollow, investors = line[:i1], line[i1 + 1:]
                if investors.strip() is '-':
                    continue
                else:
                    investors = investors.strip().strip("\"\';") #先去掉空格，再去掉符号
            except:
                continue
            for obj in investors.split(";"):
                obj = obj.strip()
                if obj is '':
                    continue
                if obj in dict1:
                    dict1[obj] += 1
                else:
                    dict1[obj] = 1
            if hasFollow is "1":
                if obj in dict2:
                    dict2[obj] += 1
                else:
                    dict2[obj] = 1
    with open("case_study_optput_1.csv", 'w') as fout:
        fout.write("Investor,# of Deals,# of Follow-Ons,Follow-On Rate\n")
        for obj in dict1:
            deals = dict1[obj]
            percent = 0
            if obj in dict2:
                percent = dict2[obj]
            fout.write('''"{0}",{1},{2},{3:.2f}%\n'''.format(
                obj, deals, percent, percent / deals*100))
    # 2
    dictInvestorIndex = {}
    listIndex2Investor = []
    matrix = []
    nextIndex = 0
    with open("fake_input2.csv", 'r') as fin:
        next(fin)
        for line in fin:
            try:
                investors = line[line.find(",") + 1:]
                if investors.strip() is '-':
                    continue
                else:
                    investors = investors.strip().strip("\"\';").split(";")
            except:
                continue
            indices = []
            for obj in investors:
                if obj is "":
                    continue
                if obj in dictInvestorIndex:
                    indices.append(dictInvestorIndex[obj])
                else:
                    dictInvestorIndex[obj] = nextIndex
                    indices.append(nextIndex)
                    if nextIndex > 0:
                        matrix.append([0] * nextIndex)
                    nextIndex += 1
                    listIndex2Investor.append(obj)
            n = len(indices)
            for i in range(n):
                for j in range(i + 1, n):
                    if indices[i] > indices[j]:
                        matrix[indices[i] - 1][indices[j]] += 1
                    if indices[j] > indices[i]:
                        matrix[indices[j] - 1][indices[i]] += 1
    with open("case_study_optput_2.csv", 'w') as fout:
        n = len(listIndex2Investor)
        fout.write("," + ",".join(listIndex2Investor) + "\n")
        for i in range(n):
            fout.write(listIndex2Investor[i])
            for j in range(n):
                if i > j:
                    fout.write(",{0}".format(matrix[i - 1][j]))
                elif i < j:
                    fout.write(",{0}".format(matrix[j - 1][i]))
                else:
                    fout.write(",-")
            fout.write('\n')
