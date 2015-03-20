package ist.meic.pa.command;

import ist.meic.pa.MethodCallEntry;
import javassist.*;
import javassist.expr.MethodCall;

import java.util.Stack;

public class ReturnCommand implements Command {
    @Override
    public void execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args) throws NotFoundException {
        MethodCallEntry calledMethod = stack.pop();
        ClassPool pool = ClassPool.getDefault();
        CtClass cc = pool.get(calledMethod.getInstanceClass().toString());
        CtMethod m = cc.getDeclaredMethod(calledMethod.getMethodName());
        CtClass returnType = m.getReturnType();
        // call parser with the return type and the args
    }
}

