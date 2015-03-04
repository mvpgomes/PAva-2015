package com.pava.shell;

import java.util.Map;

public class GetCommand implements Command {
    @Override
    public Object execute(String[] args, Map<String, Object> variables, Object lastResult) {
        if (variables.containsKey(args[0])) {
            Object var = variables.get(args[0]);
            System.out.println(var.toString());
            return var;
        } else {
            return null;
        }
    }
}
