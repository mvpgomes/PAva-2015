package com.pava.shell;

import java.util.Map;

public class ClassCommand implements Command {
    @Override
    public Object execute(String[] args, Map<String, Object> variables, Object lastResult) {
        try {
            return Class.forName(args[0]);
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        return null;
    }
}
