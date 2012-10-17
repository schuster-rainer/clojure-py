"""persistentvector_tests.py

Wednesday, March 28 2012
"""

import re
import unittest
from cStringIO import StringIO

import clojure.lang.persistentvector as pv
from clojure.lang.indexableseq import IndexableSeq
from clojure.lang.cljexceptions import (IndexOutOfBoundsException,
                                        IllegalStateException)

uobj = object()
pseudoMetaData = object()

def vecHash(v):
    """Duplicates APersistentVector.__hash__"""
    h = 1
    for e in v:
        h = 31 * h + hash(e)
    return h

class TestPersistentVector(unittest.TestCase):
    def setUp(self):
        self.printV = pv.create(pv.EMPTY, pv.EMPTY)
        self.v3 = pv.vec(["x", "y", "z"])
    # vec(), create()
    def testCreation_PASS(self):
        for k, v in testCreationMap_PASS.items():
            self.assertEqual(k, v)
    # __init__() with meta data, empty()
    def testMetaData_PASS(self):
        for k, v in testMetaDataMap_PASS.items():
            self.assertEqual(k, v)
    # __call__()
    def testCall_PASS(self):
        for k, v in testCallMap_PASS.items():
            self.assertEqual(k, v)
    def testCall_FAIL(self):
        v = pv.vec([])
        # no default argument allowed
        self.assertRaises(IndexOutOfBoundsException, v.__call__, 0)
        self.assertRaises(IndexOutOfBoundsException, v.__call__, 99)
        self.assertRaises(IndexOutOfBoundsException, v.__call__, -323)
    # nth()
    def testNth_PASS(self):
        for k, v in testNthMap_PASS.items():
            self.assertEqual(k, v)
    def testNth_FAIL(self):
        v = pv.vec([])
        # no default argument given
        self.assertRaises(IndexOutOfBoundsException, v.nth, 0)
        self.assertRaises(IndexOutOfBoundsException, v.nth, 99)
        self.assertRaises(IndexOutOfBoundsException, v.nth, -2343)
    # assocN()
    def testAssocN_PASS(self):
        for k, v in testAssocNMap_PASS.items():
            self.assertEqual(k, v)
    # assoc()
    def testAssoc_PASS(self):
        for k, v in testAssocMap_PASS.items():
            self.assertEqual(k, v)
    def testAssoc_FAIL(self):
        v = pv.vec([])
        self.assertRaises(IndexOutOfBoundsException, v.assoc, 2, uobj)
    def testAssocN_FAIL(self):
        v = pv.vec([])
        self.assertRaises(IndexOutOfBoundsException, v.assocN, 2, uobj)
    # __len__()
    def testLen_PASS(self):
        for k, v in testLenMap_PASS.items():
            self.assertEqual(k, v)
    # cons()
    def testCons_PASS(self):
        for k, v in testConsMap_PASS.items():
            self.assertEqual(k, v)
    # pop()
    def testPop_PASS(self):
        for k, v in testPopMap_PASS.items():
            self.assertEqual(k, v)
    def testPop_FAIL(self):
        v = pv.vec([])
        self.assertRaises(IllegalStateException, v.pop)

    # APersistentVector methods (__eq__ needs debugging)

    # peek()
    def testPeek_PASS(self):
        self.assertEqual(self.v3.peek(), "z")
    # seq()
    def testSeq_PASS(self):
        s = self.v3.seq()
        self.assertTrue(isinstance(s, IndexableSeq))
        self.assertEqual(len(s), 3)
        self.assertEqual(s.first(), "x")
    # very basic tests here, do the rest .clj tests
    # better (= v q)
    def test__eq___PASS(self):
        v = pv.vec(["x", "y", "z"])
        self.assertTrue(self.v3 == v)
        self.assertTrue(self.v3 == self.v3)
    # just tests the basic computation and result on []
    # hash(v)
    def test__hash___PASS(self):
        v = pv.vec([1, 2, 3])
        self.assertEqual(v.__hash__(), vecHash(v))
        self.assertEqual(pv.EMPTY.__hash__(), 1)
    # (print v)
    def testWriteAsString_PASS(self):
        csio = StringIO()
        self.printV.writeAsString(csio)
        self.assertEqual(csio.getvalue(), "[[] []]")
    # (pr v)
    def testWriteAsReplString_PASS(self):
        csio = StringIO()
        self.printV.writeAsReplString(csio)
        self.assertEqual(csio.getvalue(), "[[] []]")
    # str(v)
    def test__str___PASS(self):
        self.assertEqual(self.printV.__str__(), "[[], []]")
    # repr(v)
    def test__repr___PASS(self):
        regex = r"<clojure\.lang\.persistentvector\.PersistentVector" \
                r" object at 0x[a-fA-F0-9]+ \[\[\] \[\]\]>$"
        self.assertTrue(re.match(regex, self.printV.__repr__()))


testCreationMap_PASS = {
    # vec
    pv.vec([]): pv.EMPTY,
    pv.vec([uobj])._tail[0]: uobj,
    pv.vec([0, 0, uobj])._tail[2]: uobj,
    # create
    pv.create(): pv.EMPTY,
    pv.create(uobj)._tail[0]: uobj,
    pv.create(0, 0, uobj)._tail[2]: uobj,
    }

testMetaDataMap_PASS = {
    pv.PersistentVector(pseudoMetaData, 0, 5, pv.EMPTY_NODE, []) \
        .meta(): pseudoMetaData,
    pv.PersistentVector(pseudoMetaData, 0, 5, pv.EMPTY_NODE, []) \
        .empty().meta(): pseudoMetaData,
    }
        
testCallMap_PASS = {
    # _tail used
    pv.vec([42])(0): 42,
    pv.vec([0, 42])(1): 42,
    pv.vec([0, 0, 42])(2): 42,
    # force Node creation
    pv.vec(range(32) + [42])(32): 42,
    # larg-ish vec
    pv.vec(range(10000))(9999): 9999,
    }
    
testNthMap_PASS = {
    # in range
    pv.vec([42]).nth(0): 42,
    pv.vec([None]).nth(0): None,
    pv.vec([0, 42]).nth(1): 42,
    pv.vec([0, None]).nth(1): None,
    pv.vec([0, 0, 42]).nth(2): 42,
    pv.vec([0, 0, None]).nth(2): None,
    pv.vec(range(32) + [42]).nth(32): 42,
    pv.vec(range(32) + [None]).nth(32): None,
    # larg-ish vec
    pv.vec(range(10000)).nth(9999): 9999,
    # out of range, default value returned 
    pv.vec([0, 0, 0]).nth(3, uobj): uobj,
    # default value of None returned 
    pv.vec([0, 0, 0]).nth(3, None): None,
    }
    
testAssocNMap_PASS = {
    # modify
    pv.vec([0]).assocN(0, uobj)[0]: uobj,
    pv.vec([0]).assocN(0, None)[0]: None,
    # append
    pv.vec([]).assocN(0, uobj)[0]: uobj,
    pv.vec([]).assocN(0, None)[0]: None,
    # large-ish vec
    pv.vec(range(10000)).assocN(10000, uobj)[10000]: uobj,
    pv.vec(range(10000)).assocN(10000, None)[10000]: None,
    }

testAssocMap_PASS = {
    # modify
    pv.vec([0]).assoc(0, uobj)[0]: uobj,
    pv.vec([0]).assoc(0, None)[0]: None,
    # append
    pv.vec([]).assoc(0, uobj)[0]: uobj,
    pv.vec([]).assoc(0, None)[0]: None,
    # large-ish vec
    pv.vec(range(10000)).assoc(10000, uobj)[10000]: uobj,
    pv.vec(range(10000)).assoc(10000, None)[10000]: None,
    }

testLenMap_PASS = {
    len(pv.vec([])): 0,
    len(pv.vec([0])): 1,
    len(pv.vec([0, 0])): 2,
    len(pv.vec(range(2342))): 2342,
    }

testConsMap_PASS = {
    pv.vec([]).cons(uobj)(0): uobj,
    pv.vec([]).cons(0).cons(uobj)(1): uobj,
    pv.vec([]).cons(0).cons(None)(1): None,
    }

testPopMap_PASS = {
    pv.vec([0]).pop(): pv.EMPTY,
    len(pv.vec(range(33)).pop()): 32,
    }
