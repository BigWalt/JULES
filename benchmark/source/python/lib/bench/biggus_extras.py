#################################################################################
## This file contains extras that operate on structures from the Biggus library
#################################################################################

import biggus, numpy


#################################################################################
# Extra element-wise functions
#################################################################################
def mul(a, b):
    """
    Return the elementwise evaluation of `a * b` as a biggus array
    """    
    return biggus._Elementwise(a, b, numpy.multiply, numpy.ma.multiply)


def div(a, b):
    """
    Return the elementwise evaluation of `a / b` as a biggus array
    """    
    return biggus._Elementwise(a, b, numpy.divide, numpy.ma.divide)


def abs(a):
    """
    Return a biggus array containing the absolute values of the elements of a
    """    
    # Since element-wise expects two arrays and a function of arity 2,
    # we create a dummy array and use lambdas that ignore the 2nd argument
    return biggus._Elementwise(
        a, biggus.zeros(a.shape, a.dtype),
        lambda x, y: numpy.fabs(x), lambda x, y: numpy.ma.fabs(x)
    )
    

def maximum(a, b):
    """
    Return a biggus array containing the elementwise maximum of a and b
    """    
    return biggus._Elementwise(a, b, numpy.maximum, numpy.ma.maximum)


def minimum(a, b):
    """
    Return a biggus array containing the elementwise minimum of a and b
    """    
    return biggus._Elementwise(a, b, numpy.minimum, numpy.ma.minimum)


#################################################################################
# Adapter for biggus arrays to allow arithmetic operations to be used
#################################################################################

class WithOperators(biggus.Array):
    """
    Wrapper for biggus arrays that enables arithmetic operators to be used, and
    instance methods such as mean/max/min
    """
    
    def __init__(self, arr):
        self.__arr = arr
        
    #################################################################
    # Implement the biggus.Array interface by deferring to the
    # underlying array
    #################################################################
    @property
    def dtype(self):
        return self.__arr.dtype

    @property
    def shape(self):
        return self.__arr.shape

    def __getitem__(self, keys):
        return type(self)(self.__arr.__getitem__(keys))

    def ndarray(self):
        return self.__arr.ndarray()

    def masked_array(self):
        return self.__arr.masked_array()
    
    #################################################################
    # Implement arithmetic operators
    #################################################################
    def __add__(self, other):
        if not isinstance(other, biggus.Array):
            other = biggus.ConstantArray(self.shape, other, self.dtype)
        return type(self)(biggus.add(self, other))
    
    def __radd__(self, other):
        if not isinstance(other, biggus.Array):
            other = biggus.ConstantArray(self.shape, other, self.dtype)
        return type(self)(biggus.add(other, self))
    
    def __sub__(self, other):
        if not isinstance(other, biggus.Array):
            other = biggus.ConstantArray(self.shape, other, self.dtype)
        return type(self)(biggus.sub(self, other))
    
    def __rsub__(self, other):
        # Only allow biggus arrays as the lh operand
        if not isinstance(other, biggus.Array):
            raise NotImplementedError()
        return type(self)(biggus.sub(other, self))
    
    def __mul__(self, other):
        if not isinstance(other, biggus.Array):
            other = biggus.ConstantArray(self.shape, other, self.dtype)
        return type(self)(mul(self, other))
    
    def __rmul__(self, other):
        if not isinstance(other, biggus.Array):
            other = biggus.ConstantArray(self.shape, other, self.dtype)
        return type(self)(mul(other, self))
    
    def __div__(self, other):
        if not isinstance(other, biggus.Array):
            other = biggus.ConstantArray(self.shape, other, self.dtype)
        return type(self)(div(self, other))
    
    def __rdiv__(self, other):
        if not isinstance(other, biggus.Array):
            other = biggus.ConstantArray(self.shape, other, self.dtype)
        return type(self)(div(other, self))
    
    def __truediv__(self, other):
        return self.__div__(other)
    
    def __rtruediv__(self, other):
        return self.__rdiv__(other)
    
    ###############################################################
    # Implement aggregations as instance methods
    ###############################################################
    def abs(self):
        return type(self)(abs(self))
    
    def mean(self, axis = None, mdtol = 1):
        return type(self)(biggus.mean(self, axis, mdtol))
    
    def std(self, axis = None, ddof = 0):
        return type(self)(biggus.std(self, axis, ddof))
    
    def var(self, axis = None, ddof = 0):
        return type(self)(biggus.var(self, axis, ddof))
    
    def max(self, axis = None):
        return type(self)(biggus.max(self, axis))
    
    def min(self, axis = None):
        return type(self)(biggus.min(self, axis))
