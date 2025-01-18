package net.solostudio.dutycore.utils;

import lombok.experimental.UtilityClass;
import net.solostudio.dutycore.hooks.Webhook;
import net.solostudio.dutycore.interfaces.PlaceholderProvider;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.net.URISyntaxException;

@UtilityClass
public class EventUtils {
    public void handleEvent(@NotNull String webhookKey, @NotNull PlaceholderProvider event) {
        try {
            Webhook.sendWebhookFromString(webhookKey, event);
        } catch (IOException | URISyntaxException exception) {
            LoggerUtils.error(exception.getMessage());
        }
    }
}
