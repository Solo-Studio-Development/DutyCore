package net.solostudio.dutycore.versions;

import lombok.Getter;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.enums.VersionTypes;
import net.solostudio.dutycore.interfaces.ServerVersionSupport;
import net.solostudio.dutycore.utils.LoggerUtils;
import org.bukkit.plugin.Plugin;
import org.jetbrains.annotations.NotNull;

import java.lang.reflect.InvocationTargetException;

@Getter
public class VersionSupport {
    private final ServerVersionSupport versionSupport;

    public VersionSupport(@NotNull VersionTypes version) throws ClassNotFoundException, InstantiationException, IllegalAccessException, NoSuchMethodException, InvocationTargetException {
        if (version == VersionTypes.UNKNOWN) LoggerUtils.error("### VERSION NOT FOUND! ###");


        Class<?> clazz = Class.forName("net.solostudio.dutycore.versions.nms." + version.name() + ".ServerVersion");
        versionSupport = (ServerVersionSupport) clazz.getConstructor(Plugin.class).newInstance(DutyCore.getInstance());

        if (!versionSupport.isSupported()) {
            LoggerUtils.warn("---   VERSION IS NOT SUPPORTED YET,   ---");
            LoggerUtils.warn("The version you are using is badly");
            LoggerUtils.warn("implemented. Many features won't work.");
            LoggerUtils.warn("Please consider updating your server ");
            LoggerUtils.warn("version to a newer version. (like 1.20_R1)");
            LoggerUtils.warn("---   PLEASE READ THIS MESSAGE!   ---");
        }

        LoggerUtils.info("### Version support for {} loaded! ###", version);
    }
}
