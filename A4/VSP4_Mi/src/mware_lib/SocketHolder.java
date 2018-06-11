package mware_lib;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;

public class SocketHolder {

	private Socket socket;
	private BufferedReader reader;
	private BufferedWriter writer;

	public SocketHolder(Socket socket) throws IOException {
		this.reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
		this.writer = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()));
	}

	public Socket getSocket() {
		return this.socket;
	}

	public BufferedReader getIn() {
		return this.reader;
	}

	public BufferedWriter getOut() {
		return this.writer;
	}

	public void close() throws IOException {
		if (this.socket != null) {
			this.socket.close();
		}
	}
}
