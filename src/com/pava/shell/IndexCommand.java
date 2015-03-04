package com.pava.shell;

import java.util.Map;

public class IndexCommand implements Command {
    @Override
    public Object execute(String[] args, Map<String, Object> variables, Object lastResult) {
        if (lastResult instanceof Object[]) {
            Object[] arr = (Object[]) lastResult;
            int index = Integer.parseInt(args[0]);
            return arr[index];
        }
        return null;
    }
}
