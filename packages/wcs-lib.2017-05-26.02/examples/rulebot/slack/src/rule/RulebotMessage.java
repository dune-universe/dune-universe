package rule;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.annotations.Expose;

public class RulebotMessage {

	@Expose
	public String text;
	@Expose
	public boolean userinput;
	@Expose
	public String status;

	public JsonElement toJson() {
		final Gson g = new GsonBuilder().disableHtmlEscaping()
				.excludeFieldsWithoutExposeAnnotation().create();
		return g.toJsonTree(this);
	}

	public static RulebotMessage fromJson(JsonElement je) {
		final GsonBuilder builder = new GsonBuilder();
		final Gson gson = builder.create();

		final RulebotMessage rc = gson.fromJson(je, RulebotMessage.class);
		  
		return rc;
	}
}
