package com.pava.shell;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;

public class Main {

    public static void main(String[] args) throws IOException {
        String line;
        String[] command;

        Map<String, Object> variables = new HashMap<String, Object>();

        Map<String, Command> commands = new HashMap<String, Command>() {{
            put("Class", new ClassCommand());
            put("Set", new SetCommand());
            put("Get", new GetCommand());
            put("Index", new IndexCommand());
        }};

        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        Object lastResult = null;
        while(true){
            System.out.println("");
            line = br.readLine();
            command = line.split(" ", 0);
            System.out.println(command[0]);

            String[] commandArgs = Arrays.copyOfRange(command, 1, command.length);
            if (commands.containsKey(command[0])) {
                lastResult = commands.get(command[0]).execute(commandArgs, variables, lastResult);

            } else {
                lastResult = new GenericCommand().execute(commandArgs, variables, lastResult);
            }

        }
    }
}
