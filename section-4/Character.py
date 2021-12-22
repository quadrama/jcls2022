import re


def identify_type(id):
    if re.fullmatch("[_a-zöäüßé]+", id):
        return "dracor-id"
    elif re.fullmatch("[-_A-ZÖÄÜÉ0-9\[\]]+", id):
        return "variable"
    elif re.fullmatch('".+"', id):
        return "non-dracor-id"
    else:
        raise IOError("Character ID malformed: {}".format(id))


class Character:
    """represents characters by id and type (possible values: dracor-id, variable)"""

    def __init__(self, id):
        if isinstance(id, Character):
            self = id
        else:
            self.id = id
            self.type = identify_type(id)

    def __repr__(self):
        return "Char({})".format(self.id)

    def __eq__(self, other):
        if isinstance(other, Character):
            if self.id == other.id and self.type == other.type:
                return True
        else:
            return False
