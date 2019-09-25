class MyRational
    def initialize (numerator, denominator=1)
        if denominator == 0
            raise "MyRational initialize: the denominator cannot be zero!"
        elsif denominator < 0
            @numerator = - numerator
            @denominator = - denominator
        else
            @numerator = numerator
            @denominator = denominator
        end
        reduce
    end
    def to_s
        if @denominator == 1
            @numerator.to_s
        else
            @numerator.to_s + "/" + @denominator.to_s
        end
    end
    def + x
        xnumerator = x.numerator
        xdenominator = x.denominator
        new = MyRational.new(@numerator * xdenominator + @denominator * xnumerator, @denominator * xdenominator)
        new.reduce
        new
    end
    def * x
        xnumerator = x.numerator
        xdenominator = x.denominator
        new = MyRational.new(@numerator * xnumerator, @denominator * xdenominator)
        new.reduce
        new
    end
    protected
    # Notice here: we make the getters numerator and denominator protected, so that only from within a object whose class is MyRational(or its subclass) can
    #   we call the numerator and denominator method on another object of class MyRational.
    # (NOTE: There is always a "self"-binding in the envionment, which indicates the "current object" which we are calling method on.)
    attr_reader :numerator, :denominator
    private
    def gcd (x, y) # Invariance: x should be non-negative, y should be positive.
        if x % y == 0
            y
        else
            gcd(y, x % y)
        end
    end
    def reduce # Invariance: @denominator should be positive.
        divisor = gcd(@numerator.abs, @denominator)
        @numerator /= divisor
        @denominator /= divisor
    end
end
