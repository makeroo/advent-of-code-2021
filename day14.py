'''
import sys

with open(sys.argv[1]) as f:
    lines = f.readlines()
'''

'''
class Elem:
    def __init__(self, v):
        self.v = v
        self.n = 1
    def __str__(self):
        if self.n == 1:
            return self.v
        return f'{self.v.lower()}({self.n})'
    __repr__ = __str__

class Rules:
    def __init__(self, rulemap):
        self.left_rec = {}
        self.right_rec = {}
        self.non_rec = {}
        for k, v in rulemap.items():
            if k[0] == v:
                self.left_rec[k] = v
            elif k[1] == v:
                self.right_rec[k] = v
            else:
                self.non_rec[k] = v
    def __str__(self):
        return f'{self.left_rec}/{self.right_rec}/{self.non_rec}'

seq = [Elem(v) for v in lines[0].strip()]
rules = Rules(dict(l.strip().split(' -> ') for l in (lines[2:]) if l))

def apply_rules(seq, rules):
    couples = [seq[i:i+2] for i in range(0,len(seq)-1)]
    # m = rules.left_rec[
'''

# rules = dict(l.strip().split(' -> ') for l in (lines[2:]) if l)

def apply_rules(seq, rules):
    couples = [seq[i:i+2] for i in range(0,len(seq)-1)]
    r = ''
    for c in couples:
        r += c[0]
        r += rules[c]
        # r += c[1]
    return r + seq[-1]

def calc_freq(seq):
    couples = [seq[i:i+2] for i in range(0,len(seq)-1)]
    r = {}
    for c in couples:
        r[c] = r.get(c,0) + 1
    return r

def letters(seq):
    r = {}
    for l in seq:
        r[l] = r.get(l,0) + 1
    return r

# fseq = calc_freq(seq)

def apply_rules_f(fseq, rules):
    r = {}
    def inc(x, v):
        r[x] = r.get(x,0) + v
    for k, v in fseq.items():
        ins = rules[k]
        inc(k[0]+ins, v)
        inc(ins+k[1], v)
    return r

def letters_f(fseq, seq):
    r = {}
    def inc(k, x):
        r[k] = r.get(k, 0) + x
    for k, v in fseq.items():
        inc(k[0], v)
        # inc(k[1], v)
    inc(seq[-1], 1)
    return r

def test(seq, rules):
    n = seq
    nf = calc_freq(seq)
    for i in range(40):
        n = apply_rules(n, rules)
        nf = apply_rules_f(nf, rules)
        print(n)
        print(nf)
        if letters(n) != letters_f(nf, seq):
            print(i, ', error: ', n, nf, letters(n), letters_f(nf, seq))
            break
        print(i)
        print()

def day14star2(seq, rules):
    nf = calc_freq(seq)
    for i in range(40):
        nf = apply_rules_f(nf, rules)
    ll = letters_f(nf, seq)
    print(ll)
    return max(ll.values()) - min(ll.values())
    
def read(path):
    with open(path) as f:
        lines = f.readlines()
    seq = lines[0].strip()
    rules = dict(l.strip().split(' -> ') for l in (lines[2:]) if l)
    return seq, rules
