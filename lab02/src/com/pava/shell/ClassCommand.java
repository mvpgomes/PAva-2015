package com.pava.shell;

import java.util.Map;

public class ClassCommand implements Command {
    @Override
    public Object execute(String[] args, Map<String, Object> variables, Object lastResult) {
        try {
            Class c = Class.forName(args[0]);
            System.out.println(c.toString());
            return c;
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }
}
