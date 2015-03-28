package ist.meic.pa;

/**
 * This is a generic implementation of a tuple with two elements.
 * @param <T> the type of the first element.
 * @param <U> the type of the second element.
 */
public class Tuple<T, U> {
    private T first;
    private U second;

    public Tuple(T first, U second) {
        this.first = first;
        this.second = second;
    }

    public T getFirst() {
        return first;
    }

    public U getSecond() {
        return second;
    }
}
