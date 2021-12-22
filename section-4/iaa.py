import csv
import sys
import numba as nb
import numpy as np

from pygamma_agreement import Continuum
from pyannote.core import Segment
from pygamma_agreement import CombinedCategoricalDissimilarity
from pygamma_agreement import CategoricalDissimilarity, LambdaCategoricalDissimilarity, AbstractDissimilarity, Unit
from typing import Iterable, Callable
from pygamma_agreement import show_alignment

from Relation import Relation
from parsers import parse_annotation,is_parseable,parse_relation

dissimilarity_dec = nb.njit(nb.float32(nb.float32[:], nb.float32[:]))
undirected_relations = ['siblings', 'cousins', 'lovers', 'couple', 'engaged', 'spouses', 'identity']

def intersect(l1, l2):
  s1 = set(l1)
  s2 = set(l2)
  return len(set(s1.intersection(s2)))

def jaccard_similarity(list1, list2):
  intersection = intersect(list1, list2)
  union = (len(set(list1)) + len(set(list2))) - intersection
  return float(intersection) / union

def segment_overlap(seg1, seg2):
  return (seg1.start <= seg2.end and seg1.end >= seg2.start)# or (eg2.start <= seg1.end and seg2.end >= seg1.start)

class QTrackPositionalDissimilarity(AbstractDissimilarity):
  def __init__(self, overlap_dissim=0.1, non_overlap_factor = 0.1, delta_empty=1.0):
    self.odiss = overlap_dissim
    self.nof = non_overlap_factor
    super().__init__(delta_empty=delta_empty)

  # Abstract methods overrides
  def compile_d_mat(self) -> Callable[[np.ndarray, np.ndarray], float]:
    delta_empty = self.delta_empty
    odiss = self.odiss
    nof = self.nof
    @dissimilarity_dec
    def d_mat(unit1: np.ndarray, unit2: np.ndarray) -> float:
      if unit1[0] == unit2[0] and unit1[1] == unit2[1]:
        return 0
      elif (unit1[0] <= unit2[1] and unit1[1] >= unit2[0]):
        return odiss * delta_empty
      else:
        return abs(unit1[0] - unit2[0]) * nof * delta_empty
    return d_mat

  def d(self, unit1: Unit, unit2: Unit) -> float:
    if unit1.segment == unit2.segment:
      return 0
    elif segment_overlap(unit1.segment, unit2.segment):
      return self.odiss * self.delta_empty
    else:
      return abs(unit1.segment.start - unit2.segment.start) * self.nof * self.delta_empty

class QTrackDissimilarity(LambdaCategoricalDissimilarity):
  aspects = {
    "source": 1/6, "target": 1/6, "attribute": 1/6, "relation_c1": 1/6, "relation_c2": 1/6, "relation_name": 1/6
  }
  
  def __init__(self, labels: Iterable[str], delta_empty: float = 1.0):
    super().__init__(labels, delta_empty)

  @staticmethod
  def preprocess(str1: str, str2: str) -> dict:
    (src1, tgt1, rel1, attr1) = parse_annotation(str1)
    (src2, tgt2, rel2, attr2) = parse_annotation(str2)
    rel1 = Relation(*parse_relation(rel1))
    rel2 = Relation(*parse_relation(rel2))
    return [
      {
        'source': src1,
        'target': tgt1,
        'relation': rel1,
        'attribute': attr1
      },
      {
        'source': src2,
        'target': tgt2,
        'relation': rel2,
        'attribute': attr2
        
      }
    ]

  @staticmethod
  def compare_source(d1: dict, d2: dict) -> float:
    return 1 - jaccard_similarity(d1['source'], d2['source'])

  @staticmethod
  def compare_target(d1: dict, d2: dict) -> float:
    return 1 - jaccard_similarity(d1['target'], d2['target'])

  @staticmethod
  def compare_attribute(d1: dict, d2: dict) -> float:
    #print(f"Comparing '{d1['attribute']}' and '{d2['attribute']}'.")
    if d1['attribute'] == d2['attribute']:
      return 0
    else:
      return 1

  @staticmethod
  def compare_relation_c1(d1: dict, d2: dict) -> float:
    if d1['relation'].relation_name in undirected_relations and d1['relation'].relation_name in undirected_relations:
      return 1 - jaccard_similarity(
        d1['relation'].character1 + d1['relation'].character2, 
        d2['relation'].character1 + d2['relation'].character2
      )
    return 1 - jaccard_similarity(d1['relation'].character1, d2['relation'].character1)

  @staticmethod
  def compare_relation_c2(d1: dict, d2: dict) -> float:
    if d1['relation'].relation_name in undirected_relations and d1['relation'].relation_name in undirected_relations:
      return 0
    return 1 - jaccard_similarity(d1['relation'].character2, d2['relation'].character2)

  @staticmethod
  def compare_relation_name(d1: dict, d2: dict) -> float:
    n1 = d1['relation'].relation_name.lower()
    n2 = d2['relation'].relation_name.lower()
    #print(f"Comparing '{d1['relation'].relation_name}' and '{d2['relation'].relation_name}'.")
    if n1 == n2:
      return 0
    elif n1.replace("foster", "") == n2.replace("foster", ""):
      return 0.5
    elif n1.replace("grand", "") == n2.replace("grand", ""):
      return 0.5
    elif n1.replace("step", "") == n2.replace("step", ""):
      return 0.5
    elif n1.replace("god", "") == n2.replace("god", ""):
      return 0.5
    elif n1.replace("ex-", "") == n2.replace("ex-", ""):
      return 0.5
    elif n1.replace("-in-law", "") == n2.replace("-in-law", ""):
      return 0.5    
    else: 
      return 1
  
  @staticmethod
  def cat_dissim_func(str1: str, str2: str) -> float:
    if str1 == str2:
      return 0
    ds = QTrackDissimilarity.preprocess(str1, str2)
    dists = {
      'source': QTrackDissimilarity.compare_source(*ds),
      'target': QTrackDissimilarity.compare_target(*ds),
      'attribute': QTrackDissimilarity.compare_attribute(*ds),
      'relation_c1': QTrackDissimilarity.compare_relation_c1(*ds),
      'relation_c2': QTrackDissimilarity.compare_relation_c2(*ds),
      'relation_name': QTrackDissimilarity.compare_relation_name(*ds),
    }
    s = sum([dists[a] * QTrackDissimilarity.aspects[a] for a in QTrackDissimilarity.aspects.keys()])
    #print(QTrackDissimilarity.aspects)
    #print(dists)
    #print(s)
    return  s

def getWeights():
  return [
    {"source": 1, "target": 0, "attribute": 0, "relation_c1": 0, "relation_c2": 0, "relation_name": 0},
    {"source": 0, "target": 1, "attribute": 0, "relation_c1": 0, "relation_c2": 0, "relation_name": 0},
    {"source": 0, "target": 0, "attribute": 1, "relation_c1": 0, "relation_c2": 0, "relation_name": 0},
    {"source": 0, "target": 0, "attribute": 0, "relation_c1": 1, "relation_c2": 0, "relation_name": 0},
    {"source": 0, "target": 0, "attribute": 0, "relation_c1": 0, "relation_c2": 1, "relation_name": 0},
    {"source": 0, "target": 0, "attribute": 0, "relation_c1": 0, "relation_c2": 0, "relation_name": 1},
  ]

def getWeights2():
  return [
    {"source": 0.95, "target": 0.01, "attribute": 0.01, "relation_c1": 0.01, "relation_c2": 0.01, "relation_name": 0.01},
    {"source": 0.01, "target": 0.95, "attribute": 0.01, "relation_c1": 0.01, "relation_c2": 0.01, "relation_name": 0.01},
    {"source": 0.01, "target": 0.01, "attribute": 0.95, "relation_c1": 0.01, "relation_c2": 0.01, "relation_name": 0.01},
    {"source": 0.01, "target": 0.01, "attribute": 0.01, "relation_c1": 0.95, "relation_c2": 0.01, "relation_name": 0.01},
    {"source": 0.01, "target": 0.01, "attribute": 0.01, "relation_c1": 0.01, "relation_c2": 0.95, "relation_name": 0.01},
    {"source": 0.01, "target": 0.01, "attribute": 0.01, "relation_c1": 0.01, "relation_c2": 0.01, "relation_name": 1},
  ]

def getWeightsU():
  return [
    {"source": 1/6, "target": 1/6, "attribute": 1/6, "relation_c1": 1/6, "relation_c2": 1/6, "relation_name": 1/6},
  ]



def calculateGamma(continuum, categories, config):  
  QTrackDissimilarity.aspects = config['w']

  qtrack_dissim = QTrackDissimilarity(categories)
  qtrack_pos_dissim = QTrackPositionalDissimilarity(
    overlap_dissim=config['pos']['overlap'],
    non_overlap_factor = config['pos']['factor'])
  dissim = CombinedCategoricalDissimilarity(alpha=config['alpha'], beta=config['beta'],
    cat_dissim=qtrack_dissim, pos_dissim=qtrack_pos_dissim)

  gamma_results = continuum.compute_gamma(dissim, soft=config['soft'])

  return gamma_results

def parseFiles(*inputFileNames):
  continuum = Continuum()
  categories = set()
  for fname in inputFileNames:
    if "01.csv" in fname:
      aname = "Annotator 1"
    else:
      aname = "Annotator 2"
    with open(fname) as csvfile:
      reader = csv.DictReader(csvfile)
      for row in reader:
        if is_parseable(row['entityLabel']):
          continuum.add(aname,
            Segment(int(row['begin']), int(row['end'])),
            row['entityLabel'])
          categories.add(row['entityLabel'])
  return [continuum, categories]


def compareFiles(*inputFileNames):
  configs = [
    {'w': getWeights2()[0], 'soft':True, 'alpha':1, 'beta':0, 'pos': {'factor': 0.001, 'overlap': 0}},
    {'w': getWeights2()[0], 'soft':True, 'alpha':1, 'beta':2, 'pos': {'factor': 0.001, 'overlap': 0}},
    {'w': getWeights2()[1], 'soft':True, 'alpha':1, 'beta':2, 'pos': {'factor': 0.001, 'overlap': 0}},
    {'w': getWeights2()[2], 'soft':True, 'alpha':1, 'beta':2, 'pos': {'factor': 0.001, 'overlap': 0}},
    {'w': getWeights2()[3], 'soft':True, 'alpha':1, 'beta':2, 'pos': {'factor': 0.001, 'overlap': 0}},
    {'w': getWeights2()[4], 'soft':True, 'alpha':1, 'beta':2, 'pos': {'factor': 0.001, 'overlap': 0}},
    {'w': getWeights2()[5], 'soft':True, 'alpha':1, 'beta':2, 'pos': {'factor': 0.001, 'overlap': 0}},
    {'w': getWeightsU()[0], 'soft':True, 'alpha':1, 'beta':2, 'pos': {'factor': 0.0001, 'overlap': 0}}
  ]
  results = []
  for c in configs:
    cont, cat = parseFiles(*inputFileNames)
    results.append(calculateGamma(cont, cat, c))

  gammas = [r.gamma for r in results]
  print("{:s} & {:1.3f} & {:1.3f} & {:1.3f} & {:1.3f} & {:1.3f} & {:1.3f} & {:1.3f} & {:1.3f} \\\\".format(
    inputFileNames[0].replace('../knowledge-annotation/round-2/V1/', '').replace("_01.csv", "").replace("-", ""), *gammas))


if __name__ == "__main__":
  """
  Usage: 
  python3 iaa.py ../knowledge-annotation/round-2/V1/csv/guenderode-udohla_0?.csv
  
  Usage:
  for i in $( ls ../data/round-2/V1/csv/*01.csv); do python3 iaa.py $i ${i/01/02}; done
  """
  if len(sys.argv) == 3:
    compareFiles(sys.argv[1], sys.argv[2])
