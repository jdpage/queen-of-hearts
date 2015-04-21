import sys
import string
import random

random.seed()

def yield_word(word, first):
    word = word.lower()
    if word.endswith('?'):
        yield (word[:-1], first)
        yield (word[-1], False)
    else:
        yield (word, first)

def get_words(fname):
    punctuation = set(string.punctuation)
    punctuation.remove('\'')
    punctuation.remove('?')

    with open(fname, 'r') as fin:
        for line in fin:
            line = ''.join(c for c in line.strip() if c not in punctuation)
            word, *words = line.split()
            yield from yield_word(word, True)
            for word in words:
                yield from yield_word(word, False)

def get_starts(words):
    return [word for (word, start) in words if start]

def get_pairs(words):
    last = None
    first = None
    for word, start in words:
        if first is None:
            first = word
        if last is not None:
            yield (last, word)
        last = word
    yield (last, first)

def get_numbers(words):
    numbers = dict()
    wws = []
    counter = 0
    for word, start in words:
        if word not in numbers:
            numbers[word] = counter
            wws.append(word)
            counter += 1
    return numbers, wws

def construct_transition_table(fname):
    words = list(get_words(fname))
    starts = list(get_starts(words))
    numbers, word_index = get_numbers(words)

    transition_table = dict()
    for index, word in enumerate(word_index):
        transition_table[word] = [0] * len(word_index)

    for left, right in get_pairs(words):
        transition_table[left][numbers[right]] += 1

    return word_index, transition_table, starts

def next_state(state, word_index, transition_table, starts):
    choices = transition_table[state]
    choice = random.randrange(sum(choices))
    for k, c in enumerate(choices):
        choice -= c
        if choice < 0:
            return word_index[k]
    raise "whoops"

def extract_sentence(length, word_index, transition_table, starts):
    state = random.choice(starts)
    for _ in range(length):
        yield state
        state = next_state(state, word_index, transition_table, starts)

def map_passion(passion):
    passion = int(passion * 5)
    if passion == 0:
        return "murmur"
    elif passion == 1:
        return "gently"
    elif passion == 2:
        return "speak"
    elif passion == 3:
        return "loudly"
    elif passion == 4:
        return "yell"
    else:
        return "scream"

tt = construct_transition_table(sys.argv[1])

for line in sys.stdin:
    re, b, im, d, mod, *cmds = line.strip().split()
    with open('score{0}i{1}m{2}.txt'.format(re, im[:-1], mod[:-1]), 'w') as fout:
        fout.write('{0} + {1} (mod {2}\n\n'.format(re, im, mod))
        for length, passion in zip(cmds[::2], cmds[1::2]):
            length = int(length)
            passion = float(passion)
            fout.write('. ({0}) '.format(map_passion(passion)))
            fout.write(' '.join(extract_sentence(length, *tt)))




