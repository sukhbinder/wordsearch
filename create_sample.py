import platform
import random
from subprocess import run


def get_app_name():
    if "darwin" in platform.platform().lower():
        app_name = r"./dist/wordsearch_mac {0} > samples/words_{1}.txt"
    else:
        app_name = r"./dist/wordsearch.exe {0} > samples/words_{1}.txt"
    return app_name

def get_words():
    with open("words", "r") as f_in:
        words = f_in.readlines()
    words = [e.strip() for e in words] 
    random.shuffle(words)
    return words

def main():
    app_name = get_app_name()
    words = get_words()
    num = len(words)
    for i, j in enumerate(range(0, num, 10),1):
        sentence = " ".join(words[j: j+10])
        run_args = [app_name.format(sentence, i)]
        run(run_args, shell=True,  universal_newlines=True)
    print("\aPuzzles created. Please check samples folder!")


if __name__ == "__main__":
    main()
