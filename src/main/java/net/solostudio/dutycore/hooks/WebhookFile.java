package net.solostudio.dutycore.hooks;

import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.managers.ConfigurationManager;

import java.io.File;

public class WebhookFile extends ConfigurationManager {
    public WebhookFile() {
        super(DutyCore.getInstance().getDataFolder().getPath() + File.separator + "settings", "webhook");
        save();
    }
}
