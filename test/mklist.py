import json

xs = [
    "future.lmb",
    "lock.lmb",
    "pat_constr.lmb",
    "pat_constr2.lmb",
    "pat_constr3.lmb",
    "pat_constr4.lmb",
    "unbound.lmb",
    "useafterfree.lmb",
    "weird.lmb",
    "weird2.lmb",
    "weird3.lmb",
    "weird4.lmb",
]

def entry(x):
    return json.dumps({"name": "", "filename": x, "exit_code": 1})

print("\n".join([entry(x) for x in xs]))

