import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Main {
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

		System.out.println("Give me a name (HelloWorld or GoodbyeWorld): ");
		String className = in.readLine();

		try {
			Class clazz = Class.forName(className);
			Message instance = (Message) clazz.newInstance();
			instance.say();
		} catch (ClassNotFoundException e) {
			System.out.println("Class \"" + className + "\" not found.");
		} catch (InstantiationException e) {
			System.out.println("Can't create instance of class \"" + className + "\"");
		} catch (IllegalAccessException e) {
			System.out.println("Can't access method \"say()\".");
		}
	}
}
