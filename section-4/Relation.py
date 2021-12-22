class Relation:
    """represents the relation between two characters"""

    def __init__(self, relation, character1, character2):
        self.relation_name = relation
        self.character1 = character1
        self.character2 = character2

    def __repr__(self):
        return "Rel({} is in relation {} to {})".format(
            self.character1, self.relation_name, self.character2
        )

    def __eq__(self, other):
        if isinstance(other, Relation):
            if (
                self.relation_name == other.relation_name
                and self.character1 == other.character1
                and self.character2 == other.character2
            ):
                return True
        else:
            return False
