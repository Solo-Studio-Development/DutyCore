package net.solostudio.dutycore;

import com.github.Anon8281.universalScheduler.UniversalScheduler;
import com.github.Anon8281.universalScheduler.scheduling.schedulers.TaskScheduler;
import lombok.Getter;
import net.solostudio.dutycore.config.Config;
import net.solostudio.dutycore.enums.LanguageTypes;
import net.solostudio.dutycore.enums.keys.ConfigKeys;
import net.solostudio.dutycore.hooks.WebhookFile;
import net.solostudio.dutycore.language.Language;
import net.solostudio.dutycore.utils.LoggerUtils;
import org.bukkit.plugin.java.JavaPlugin;
import revxrsal.zapper.ZapperJavaPlugin;

import java.lang.reflect.InvocationTargetException;

import static net.solostudio.dutycore.utils.StartingUtils.initialize;
import static net.solostudio.dutycore.utils.StartingUtils.saveResourceIfNotExists;

public final class DutyCore extends ZapperJavaPlugin {
    @Getter private static DutyCore instance;
    @Getter private TaskScheduler scheduler;
    @Getter private Language language;
    @Getter private WebhookFile webhookFile;
    private Config config;

    @Override
    public void onLoad() {
        instance = this;
        scheduler = UniversalScheduler.getScheduler(this);
    }

    @Override
    public void onEnable() {
        saveDefaultConfig();
        initializeComponents();
        // database

        try {
            initialize();
        } catch (ClassNotFoundException | InvocationTargetException | InstantiationException | NoSuchMethodException | IllegalAccessException exception) {
            LoggerUtils.error(exception.getMessage());
        }

        //hooks

    }

    @Override
    public void onDisable() {
        // Plugin shutdown logic
    }

    public Config getConfiguration() {
        return config;
    }

    private void initializeComponents() {
        config = new Config();

        saveResourceIfNotExists("locales/messages_en.yml");
        saveResourceIfNotExists("locales/messages_hu.yml");
        saveResourceIfNotExists("config.yml");
        saveResourceIfNotExists("settings/webhook.yml");

        language = new Language("messages_" + LanguageTypes.valueOf(ConfigKeys.LANGUAGE.getString()));
        webhookFile = new WebhookFile();

        getConfiguration().updateConfigWithDefaults();
        getLanguage().updateConfigWithDefaults();
        getWebhookFile().updateConfigWithDefaults();
    }
}
