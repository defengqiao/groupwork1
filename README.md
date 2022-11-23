# A Markov Text Model

This practical is about creating a computer generated text.

The idea is to use a 2nd order Markov model — that is a model in which we generate words sequentially, with the each word being drawn with a probability dependent on the words preceding it. The probabilities are obtained by training the model on the actual text. That is by simply tabulating the frequency with which each word follows any other pair of words.
