package net.solostudio.dutycore;

import com.github.Anon8281.universalScheduler.UniversalScheduler;
import com.github.Anon8281.universalScheduler.scheduling.schedulers.TaskScheduler;
import lombok.Getter;
import net.solostudio.dutycore.config.Config;
import net.solostudio.dutycore.database.DatabaseProxy;
import net.solostudio.dutycore.database.MySQL;
import net.solostudio.dutycore.enums.DatabaseTypes;
import net.solostudio.dutycore.enums.LanguageTypes;
import net.solostudio.dutycore.enums.keys.ConfigKeys;
import net.solostudio.dutycore.hooks.WebhookFile;
import net.solostudio.dutycore.interfaces.DutyDatabase;
import net.solostudio.dutycore.language.Language;
import net.solostudio.dutycore.utils.LoggerUtils;
import org.bukkit.plugin.java.JavaPlugin;
import revxrsal.zapper.ZapperJavaPlugin;

import java.lang.reflect.InvocationTargetException;
import java.sql.SQLException;
import java.util.Objects;

import static net.solostudio.dutycore.utils.StartingUtils.initialize;
import static net.solostudio.dutycore.utils.StartingUtils.saveResourceIfNotExists;

public final class DutyCore extends ZapperJavaPlugin {
    @Getter private static DutyCore instance;
    @Getter private TaskScheduler scheduler;
    @Getter private Language language;
    @Getter private WebhookFile webhookFile;
    @Getter private static DutyDatabase database;
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
        initializeDatabaseManager();

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

    private void initializeDatabaseManager() {
        try {
            DutyDatabase databaseInstance;
            switch (DatabaseTypes.valueOf(ConfigKeys.DATABASE.getString().toUpperCase())) {
                case MYSQL -> {
                    LoggerUtils.info("### MySQL support found! Starting to initialize it... ###");
                    databaseInstance = new MySQL(Objects.requireNonNull(getConfiguration().getSection("database.mysql")));
                    databaseInstance.createTable();
                    LoggerUtils.info("### MySQL database has been successfully initialized! ###");
                }
                //case H2 -> {
                //    LoggerUtils.info("### H2 support found! Starting to initialize it... ###");
                //    databaseInstance = new H2();
                //    databaseInstance.createTable();
                //    LoggerUtils.info("### H2 database has been successfully initialized! ###");
                //}
                default -> throw new SQLException("Unsupported database type!");
            }

            database = DatabaseProxy.createProxy(DutyDatabase.class, databaseInstance);
        } catch (SQLException exception) {
            LoggerUtils.error("Database initialization failed: {}", exception.getMessage());
        }
    }
}
