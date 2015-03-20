package ist.meic.pa.parser;

public class FloatParser extends Parser {
    @Override
    public Object parse(String value) {
        return Float.parseFloat(value);
    }
}
