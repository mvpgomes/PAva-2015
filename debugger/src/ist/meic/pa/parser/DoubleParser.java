package ist.meic.pa.parser;

public class DoubleParser implements Parser {
    @Override
    public Object parse(String value) {
        return Double.parseDouble(value);
    }
}
