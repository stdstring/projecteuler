// MutableArrayNif.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "erl_nif.h"

#include <cstring>
#include <stdexcept>

static int Load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

//-spec create(Size :: integer(), DefaultValue :: binary()) -> MutableArrayHolder().
static ERL_NIF_TERM Create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

//-spec get(Array :: MutableArrayHolder(), Index :: non_neg_integer()) -> binary().
static ERL_NIF_TERM GetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

//-spec set(Array :: MutableArrayHolder(), Index :: non_neg_integer(), Value :: binary()) -> MutableArrayHolder().
static ERL_NIF_TERM SetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc NifFuncs[] =
{
    { "create", 2, Create },
    { "get", 2, GetCellValue },
    { "set", 3, SetCellValue }
};

ERL_NIF_INIT(mutable_array, NifFuncs, Load, nullptr, nullptr, nullptr)

struct MutableArrayItem
{
public:
    ErlNifBinary Data;

    MutableArrayItem()
    {
        Data.data = nullptr;
        Data.size = 0;
    }

    ~MutableArrayItem()
    {
        if (Data.size > 0)
            enif_release_binary(&Data);
    }
};

struct MutableArrayHolder
{
public:
    MutableArrayItem *Storage;
    int Size;
    MutableArrayItem DefaultValue;

    /*~MutableArrayHolder()
    {
        delete[] Storage;
    }*/
};

static ErlNifResourceType *MutableArrayHolderType = nullptr;

static void MutableArrayHolderDtor(ErlNifEnv* env, void* obj)
{
    //delete obj;
    delete[] static_cast<MutableArrayHolder*>(obj)->Storage;
}

static int Load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    MutableArrayHolderType = enif_open_resource_type(env, "mutable_array", "MutableArrayHolder", MutableArrayHolderDtor, ErlNifResourceFlags::ERL_NIF_RT_CREATE, nullptr);
    return 0;
}

//-spec create(Size :: integer(), DefaultValue :: binary()) -> MutableArrayHolder().
static ERL_NIF_TERM Create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int size = 0;
    if (!enif_get_int(env, argv[0], &size))
        return enif_make_badarg(env);
        //throw std::logic_error("Bad Size value");
    if (size <= 0)
        return enif_make_badarg(env);
    ErlNifBinary defaultValue;
    if (!enif_inspect_binary(env, argv[1], &defaultValue))
        return enif_make_badarg(env);
        //throw std::logic_error("Bad DefaultValue value");
    MutableArrayHolder *mutableArray = static_cast<MutableArrayHolder*>(enif_alloc_resource(MutableArrayHolderType, sizeof(MutableArrayHolder)));
    mutableArray->Size = size;
    if (!enif_alloc_binary(defaultValue.size, &mutableArray->DefaultValue.Data))
        return enif_raise_exception(env, enif_make_atom(env, "internal_error"));
        //throw std::logic_error("Bad DefaultValue memory allocation");
    std::memcpy(mutableArray->DefaultValue.Data.data, defaultValue.data, defaultValue.size);
    mutableArray->Storage = new MutableArrayItem[size];
    ERL_NIF_TERM term = enif_make_resource(env, mutableArray);
    // resource now only owned by "Erlang"
    enif_release_resource(mutableArray);
    return term;
}

//-spec get(Array :: MutableArrayHolder(), Index :: non_neg_integer()) -> binary().
static ERL_NIF_TERM GetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* rawResource = nullptr;
    if (!enif_get_resource(env, argv[0], MutableArrayHolderType, &rawResource))
        return enif_make_badarg(env);
        //throw std::logic_error("Bad Array value");
    MutableArrayHolder *mutableArray = static_cast<MutableArrayHolder*>(rawResource);
    int index = 0;
    if (!enif_get_int(env, argv[1], &index))
        return enif_make_badarg(env);
    //throw std::logic_error("Bad Index value");
    if ((index < 0) || (index >= mutableArray->Size))
        return enif_make_badarg(env);
    MutableArrayItem &data = mutableArray->Storage[index].Data.data == nullptr ? mutableArray->DefaultValue : mutableArray->Storage[index];
    //ERL_NIF_TERM term;
    //unsigned char *dest = enif_make_new_binary(env, data.Data.size, &term);
    //std::memcpy(dest, data.Data.data, data.Data.size);
    ErlNifBinary result;
    enif_alloc_binary(data.Data.size, &result);
    std::memcpy(result.data, data.Data.data, data.Data.size);
    ERL_NIF_TERM term = enif_make_binary(env, &result);
    return term;
}

//-spec set(Array :: MutableArrayHolder(), Index :: non_neg_integer(), Value :: binary()) -> MutableArrayHolder().
static ERL_NIF_TERM SetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* rawResource = nullptr;
    if (!enif_get_resource(env, argv[0], MutableArrayHolderType, &rawResource))
        return enif_make_badarg(env);
        //throw std::logic_error("Bad Array value");
    MutableArrayHolder *mutableArray = static_cast<MutableArrayHolder*>(rawResource);
    int index = 0;
    if (!enif_get_int(env, argv[1], &index))
        return enif_make_badarg(env);
        //throw std::logic_error("Bad Index value");
    if ((index < 0) || (index >= mutableArray->Size))
        return enif_make_badarg(env);
    ErlNifBinary value;
    if (!enif_inspect_binary(env, argv[2], &value))
        //throw std::logic_error("Bad Value value");
        return enif_make_badarg(env);
    if (mutableArray->Storage[index].Data.size > 0)
        enif_release_binary(&mutableArray->Storage[index].Data);
    if (!enif_alloc_binary(value.size, &mutableArray->Storage[index].Data))
        return enif_raise_exception(env, enif_make_atom(env, "internal_error"));
        //throw std::logic_error("Bad Value memory allocation");
    std::memcpy(mutableArray->Storage[index].Data.data, value.data, value.size);
    return argv[0];
}

/*
const char* ModuleName = "mutable_array";

struct MutableArrayItem
{
public:
    int Size;
    unsigned char *Data;

    MutableArrayItem() : Size(0), Data(nullptr) {}
    ~MutableArrayItem() { delete[] Data; }
};

struct MutableArrayHolder
{
public:
    MutableArrayItem *Storage;
    int Size;
    MutableArrayItem DefaultValue;

    ~MutableArrayHolder()
    {
        delete[] Storage;
    }
};


static ErlNifResourceType *MutableArrayHolderType = nullptr;

static void MutableArrayHolderDtor(ErlNifEnv* env, void* obj)
{
    delete obj;
}

static int Load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    MutableArrayHolderType = enif_open_resource_type(env, ModuleName, "MutableArrayHolder", MutableArrayHolderDtor, ErlNifResourceFlags::ERL_NIF_RT_CREATE, nullptr);
    return 0;
}

//-spec create(Size :: integer(), DefaultValue :: binary()) -> MutableArrayHolder().
static ERL_NIF_TERM Create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int size = 0;
    if (!enif_get_int(env, argv[0], &size))
        throw std::logic_error("Bad Size value");
    ErlNifBinary defaultValue;
    if (!enif_inspect_binary(env, argv[1], &defaultValue))
        throw std::logic_error("Bad DefaultValue value");
    MutableArrayHolder *mutableArray = static_cast<MutableArrayHolder*>(enif_alloc_resource(MutableArrayHolderType, sizeof(MutableArrayHolder)));
    mutableArray->Size = size;
    mutableArray->DefaultValue.Size = defaultValue.size;
    mutableArray->DefaultValue.Data = new unsigned char[defaultValue.size];
    std::memcpy(mutableArray->DefaultValue.Data, defaultValue.data, defaultValue.size);
    mutableArray->Storage = new MutableArrayItem[size];
    ERL_NIF_TERM term = enif_make_resource(env, mutableArray);
    // resource now only owned by "Erlang"
    enif_release_resource(mutableArray);
    return term;
}

//-spec get(Array :: MutableArrayHolder(), Index :: non_neg_integer()) -> binary().
static ERL_NIF_TERM GetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* rawResource = nullptr;
    if (!enif_get_resource(env, argv[0], MutableArrayHolderType, &rawResource))
        throw std::logic_error("Bad Array value");
    MutableArrayHolder *mutableArray = static_cast<MutableArrayHolder*>(rawResource);
    int index = 0;
    if (!enif_get_int(env, argv[1], &index))
        throw std::logic_error("Bad Index value");
    MutableArrayItem &data = mutableArray->Storage[index].Data == nullptr ? mutableArray->DefaultValue : mutableArray->Storage[index];
    // unsigned char *enif_make_new_binary(ErlNifEnv* env, size_t size, ERL_NIF_TERM* termp)
    ERL_NIF_TERM term;
    unsigned char *dest = enif_make_new_binary(env, data.Size, &term);
    std::memcpy(dest, data.Data, data.Size);
    return term;
}

//-spec set(Array :: MutableArrayHolder(), Index :: non_neg_integer(), Value :: binary()) -> MutableArrayHolder().
static ERL_NIF_TERM SetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* rawResource = nullptr;
    if (!enif_get_resource(env, argv[0], MutableArrayHolderType, &rawResource))
        throw std::logic_error("Bad Array value");
    MutableArrayHolder *mutableArray = static_cast<MutableArrayHolder*>(rawResource);
    int index = 0;
    if (!enif_get_int(env, argv[1], &index))
        throw std::logic_error("Bad Index value");
    ErlNifBinary value;
    if (!enif_inspect_binary(env, argv[2], &value))
        throw std::logic_error("Bad Value value");
    delete[] mutableArray->Storage[index].Data;
    mutableArray->Storage[index].Size = value.size;
    mutableArray->Storage[index].Data = new unsigned char[value.size];
    std::memcpy(mutableArray->Storage[index].Data, value.data, value.size);
    return argv[2];
}*/
