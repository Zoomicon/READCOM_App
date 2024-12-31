::See https://github.com/idzresearch/aab-to-apk-converter
::Get latest bundletool.jar from https://github.com/google/bundletool/releases

@set /p PWD_KEYSTORE="Keystore password: "
@set /p PWD_KEY="Key password: "

del READCOM_App.apks

java -jar bundletool.jar build-apks --bundle=Release\READCOM_App\bin\READCOM_App.aab --output=READCOM_App.apks --overwrite --mode=universal --ks=%UserProfile%\OneDrive\Zoomicon\Android\Keystore.keystore --ks-pass=pass:%PWD_KEYSTORE% --ks-key-alias=readcom --key-pass=pass:%PWD_KEY%

echo Open the .apks with a ZIP extractor (or rename to .zip) and get the universal .apk from there (should rename to READCOM_App.universal.apk). Can also call touch on the .apk filename else it has very old/default date

@pause
