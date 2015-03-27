package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import ist.meic.pa.Tuple;
import ist.meic.pa.parser.*;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

public abstract class Command {
    public abstract Tuple<Boolean, Object> execute(Stack<MethodCallEntry> stack, String[] args, Throwable t) throws Throwable;
}
