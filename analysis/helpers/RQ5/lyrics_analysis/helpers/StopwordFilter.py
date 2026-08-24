import re
from spacy.lang.en.stop_words import STOP_WORDS as SPACY_STOP_WORDS
from spacy.lang.de.stop_words import STOP_WORDS as SPACY_STOP_WORDS_DE

_NUM_RE = re.compile(r"^\d+\S*$")


class StopwordFilter:
    """Filter n-grams that consist entirely of stopwords."""

    def __init__(self):
        """Initialize filter with stopword set."""
        self.stopwords = SPACY_STOP_WORDS.copy()
        self.stopwords.update(SPACY_STOP_WORDS_DE)
        # Keep custom additions
        self.stopwords.update(
            [
                "can't",
                "could",
                "could've",
                "got",
                "must",
                "that's",
                "there's",
            ]
        )

    def is_stopword(self, token: str) -> bool:
        """Check if a single token is a stopword or numeric."""
        return token.lower() in self.stopwords or bool(_NUM_RE.match(token))

    def is_stopword_only(self, ngram: str) -> bool:
        """Check if n-gram consists only of stopwords.

        Args:
            ngram: The n-gram string to check.

        Returns:
            True if all tokens in n-gram are stopwords, False otherwise.
        """
        tokens = ngram.lower().split()
        return all(self.is_stopword(token) for token in tokens)

    def filter_ngrams(self, ngrams: set[str]) -> set[str]:
        """Remove n-grams that consist entirely of stopwords.

        Args:
            ngrams: Set of n-gram strings to filter.

        Returns:
            Set of n-grams with stopword-only entries removed.
        """
        ngrams_to_remove = set()
        for ngram in ngrams:
            tokens = ngram.lower().split()
            if all(self.is_stopword(token) for token in tokens):
                ngrams_to_remove.add(ngram)
        return ngrams - ngrams_to_remove
