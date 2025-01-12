package net.solostudio.dutycore.config;

import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.managers.ConfigurationManager;

public class Config extends ConfigurationManager {
    public Config() {
        super(DutyCore.getInstance().getDataFolder().getPath(), "config");
        save();
    }
}
