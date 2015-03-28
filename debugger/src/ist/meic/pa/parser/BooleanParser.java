package ist.meic.pa.parser;

public class BooleanParser implements Parser {
    @Override
    public Object parse(String value) {
        return Boolean.parseBoolean(value);
    }
}
