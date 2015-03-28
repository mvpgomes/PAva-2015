package ist.meic.pa.parser;

public class LongParser implements Parser {
    @Override
    public Object parse(String value) {
        return Long.parseLong(value);
    }
}
