package rule;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;

public class RulebotConversation {

	private Process process;
	private OutputStream stdin;
	private InputStream stderr;
	private InputStream stdout;
	private BufferedReader reader;
	private BufferedWriter writer;
	private boolean initialized=false;
	private String credFile;
	private String wsConfig;

	private static final Gson gson = new GsonBuilder().setPrettyPrinting()
			.create();
	private RulebotMessage readLine() throws IOException {
		String line = reader.readLine();
		System.out.println ("Stdout: '" + line + "'");
		JsonElement je = gson.fromJson(line,JsonElement.class);
	    return RulebotMessage.fromJson(je);
	}
	private void writeLine(String userInput) throws IOException {
		String fixedUserInput = userInput.replaceAll("“", "\"");
		if (initialized) {
			System.out.println ("Stdin: '" + fixedUserInput + "'");
			this.writer.write(fixedUserInput);
			this.writer.newLine();
			this.writer.flush();
		} else {
			return; // Drop this message, since rulebot is not listening yet
		}
	}
	public List<RulebotMessage> send(String userInput) throws IOException {
		// TODO Auto-generated method stub
		writeLine(userInput);
		List<RulebotMessage> res = new ArrayList<RulebotMessage>();
		RulebotMessage msg = readLine();
		while (!msg.userinput && !msg.status.equals("done")) { // Accumulate rulebot messages until it expects user input
			res.add(msg);
			msg = readLine();
		}
		// Stop and wait for user input
		if (!initialized) {
			initialized = true; // Make sure to let the system know that rulebot is initialized
		}
		if (msg.status.equals("done")) {
			init(); // Reinitialize at end of conversation
		}
		return res;
	}
	
	private void init() throws IOException {
//		ProcessBuilder builder = new ProcessBuilder("../src/r_rulebot","-wcs-cred","../cred.json","-ws-config","../wsconfig.json","-wcs","rml","-slack-io","-slack-log","/Users/js/rulebot.log");
		ProcessBuilder builder = new ProcessBuilder("../src/r_rulebot","-wcs-cred",this.credFile,"-ws-config",this.wsConfig,"-wcs","rml","-slack-io");
		builder.redirectErrorStream(true);
		this.process = builder.start();
		this.stdin = process.getOutputStream ();
		this.stderr = process.getErrorStream ();
		this.stdout = process.getInputStream ();
		this.reader = new BufferedReader (new InputStreamReader(this.stdout));
		this.writer = new BufferedWriter(new OutputStreamWriter(this.stdin));
		initialized = false;
	}
	
	RulebotConversation(String credFile, String wsConfig) throws IOException {
		this.credFile = credFile;
		this.wsConfig = wsConfig;
		init();
	}

}
