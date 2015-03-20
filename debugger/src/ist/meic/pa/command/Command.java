package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import javassist.NotFoundException;

import java.util.Stack;

public interface Command {
    void execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args) throws NotFoundException;
}
