package ist.meic.pa.parser;

public class IntegerParser implements Parser {
    @Override
    public Object parse(String value) {
        return Integer.parseInt(value);
    }
}
