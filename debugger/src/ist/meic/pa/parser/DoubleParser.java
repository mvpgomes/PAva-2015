package ist.meic.pa.parser;

public class DoubleParser extends Parser {
    @Override
    public Object parse(String value) {
        return Double.parseDouble(value);
    }
}
