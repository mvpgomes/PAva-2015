package ist.meic.pa.command;

import ist.meic.pa.Debugger;
import ist.meic.pa.MethodCallEntry;
import javassist.*;

import java.util.Stack;

public class ReturnCommand implements Command {
    @Override
    public Object execute(Stack<MethodCallEntry> stack, Object calledObject, String[] args) throws NotFoundException {
        MethodCallEntry calledMethod = stack.pop();
        ClassPool pool = ClassPool.getDefault();
        CtClass cc = pool.get(calledMethod.getInstanceClass().toString());
        CtMethod m = cc.getDeclaredMethod(calledMethod.getMethodName());
        CtClass returnType = m.getReturnType();
        //TODO: parse input
        return Debugger.getParameterParser(returnType.getSimpleName()).parse(args[0]);
    }
}

