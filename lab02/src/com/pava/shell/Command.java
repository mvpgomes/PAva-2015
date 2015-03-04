package com.pava.shell;

import java.util.Map;

public interface Command {
    Object execute(String[] args, Map<String, Object> variables, Object lastClass);
}
