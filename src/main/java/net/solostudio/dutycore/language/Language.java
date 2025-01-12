package net.solostudio.dutycore.language;

import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.managers.ConfigurationManager;
import org.jetbrains.annotations.NotNull;

import java.io.File;

public class Language extends ConfigurationManager {
    public Language(@NotNull String name) {
        super(DutyCore.getInstance().getDataFolder().getPath() + File.separator + "locales", name);
        save();
    }
}
