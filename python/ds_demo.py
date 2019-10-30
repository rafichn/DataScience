def checkLanguage(lang):
    lang = lang.lower()
    if(lang == "r"):
        msg = "We will use R extensively on machine learning!"
    elif(lang == "python"):
        msg = "We will use Python extensively on machine learning and deep learning!"
    else:
        msg = "Sorry, we will not use %s on machine learning!" % lang
    print(msg)