import re
from Character import Character


def is_parseable(annotation):
    """Returns True if the annotation string can be parsed, False otherwise"""
    try:
        source, target, knowledge, attribute = parse_annotation(annotation)
        # moved this call into parse_annotation
        # source, target = expand(source), expand(target)
        if re.match("transfer", knowledge):
            (
                source_nested,
                target_nested,
                knowledge,
                attribute,
            ) = parse_annotation(knowledge)
            source_nested, target_nested = expand(source_nested), expand(target_nested)
            for c in source_nested + target_nested:
                c = Character(c)
        if re.match("!?knowledge", knowledge):
            target_nested, knowledge, attribute_nested = parse_knowledge(knowledge)
            target_nested = expand(target_nested)
            for c in target_nested:
                c = Character(c)
        relation, character1, character2 = parse_relation(knowledge)
        character1, character2 = expand(character1), expand(character2)
        try:
          for c in source + target + character1 + character2:
              c = Character(c)
        except TypeError:
          print(f"TypeError: {source} {target} {character1} {character2}")
    except IOError:
        return False
    return True


def parse_annotation(annotation):
    """Parses a given annotation string into its components."""

    regex = re.compile(
        r"^\s*transfer\s*\(\s*(.+?),\s*(\[.+?\]|.+?),\s*(.+\(.+\)\s*)(.*,\s*(.*?))?\s*\)\s*$"
    )
    match = re.fullmatch(regex, annotation)
    if match:
        source = match.group(1)
        target = match.group(2)
        knowledge = match.group(3)
        if match.group(4):
            attribute = match.group(5)
        else:
            attribute = None
    else:
        raise IOError(
            "CSV file malformed. Couldn't parse transfer predicate for annotation: {}".format(
                annotation
            )
        )
    # check if target is a list and split up
    # for simplicity, target is always returned as a list
    # Not sure if this is the best option
    target = expand(target)
    source = expand(source)
    return source, target, knowledge, attribute


def parse_relation(string):
    """parses the relation string and returns its components"""
    regex = re.compile(r"(.*?)\((\[.+?\]|.+?), ?(\[.+?\]|.+?)\)")
    match = re.fullmatch(regex, string)
    if match:
        relation = match.group(1)
        character1 = match.group(2)
        character2 = match.group(3)

        return relation, character1, character2

    else:
        raise IOError(
            "CSV file malformed. Couldn't parse transfer predicate for annotation: {}".format(
                string
            )
        )


def parse_knowledge(string):

    regex = re.compile(
        r"^\s*!?knowledge\s*\(\s*(\[.+?\]|.+?),\s*(.+\(.+\)\s*)(.*,\s*(.*?))?\s*\)\s*$"
    )
    match = re.fullmatch(regex, string)
    if match:
        target = match.group(1)
        knowledge = match.group(2)
        if match.group(3):
            attribute = match.group(3)
        else:
            attribute = None
    else:
        raise IOError(
            "CSV file malformed. Couldn't parse transfer predicate for annotation: {}".format(
                string
            )
        )

    return target, knowledge, attribute


def expand(target_string):
    """converts a string of one or more characters to a list of character ids"""
    if isinstance(target_string, list):
      return target_string
    if not isinstance(target_string, str):
        print(target_string)
    # faster without re
    if target_string.startswith("[") and target_string.endswith("]"):
        return [t.strip() for t in target_string[1:-1].split(",")]
    else:
        return [target_string.strip()]
