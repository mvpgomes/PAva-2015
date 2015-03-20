package ist.meic.pa.parser;

public class ShortParser extends Parser {
    @Override
    public Object parse(String value) {
        return Short.parseShort(value);
    }
}
